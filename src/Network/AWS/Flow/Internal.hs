{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}

module Network.AWS.Flow.Internal
  ( runFlowT
  , runDecide
  , throwStringError
  , hoistStringEither
  , maybeToFlowError
  , registerDomainAction
  , registerActivityTypeAction
  , registerWorkflowTypeAction
  , startWorkflowExecutionAction
  , pollForActivityTaskAction
  , respondActivityTaskCompletedAction
  , respondActivityTaskFailedAction
  , pollForDecisionTaskAction
  , respondDecisionTaskCompletedAction
  , scheduleActivityTaskDecision
  , completeWorkflowExecutionDecision
  , startTimerDecision
  , continueAsNewWorkflowExecutionDecision
  ) where

import Control.Applicative         ( (<$>), (<*>) )
import Control.Lens                ( (^.), (.~), (&) )
import Control.Monad               ( msum, mzero, liftM )
import Control.Monad.Base          ( MonadBase, liftBase, liftBaseDefault )
import Control.Monad.Except        ( MonadError, ExceptT, runExceptT, throwError )
import Control.Monad.IO.Class      ( MonadIO )
import Control.Monad.Logger        ( LogStr, runLoggingT )
import Control.Monad.Reader        ( MonadReader, ReaderT, ask, asks, local, runReaderT )
import Control.Monad.Trans.AWS     ( AWST, Env, Error, paginate, send, send_, runAWST )
import Control.Monad.Trans.Class   ( MonadTrans, lift )
import Control.Monad.Trans.Control ( MonadBaseControl
                                   , MonadTransControl
                                   , StM
                                   , StT
                                   , ComposeSt
                                   , liftBaseWith
                                   , liftWith
                                   , defaultLiftBaseWith
                                   , defaultRestoreM
                                   , restoreM
                                   , restoreT )
import Data.Aeson                  ( FromJSON, Value(..), parseJSON, (.:) )
import Data.Conduit                ( ($$) )
import Data.Conduit.List           ( consume )
import Data.HashMap.Strict         ( fromList, lookup )
import Network.AWS.SWF
import Network.AWS.Flow.Types
import Prelude              hiding ( lookup )
import Safe                        ( headMay )

-- FlowT

runFlowT :: MonadIO m => FlowEnv -> FlowT m a -> m (Either FlowError a)
runFlowT e (FlowT k) = runExceptT (runReaderT (runLoggingT k l) e) where
  l = const . const . const $ feLogger e

instance MonadBase b m => MonadBase b (FlowT m) where
    liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (FlowT m) where
    type StM (FlowT m) a = ComposeSt FlowT m a

    liftBaseWith = defaultLiftBaseWith

    restoreM = defaultRestoreM

instance MonadTrans FlowT where
    lift = FlowT . lift . lift . lift

instance MonadTransControl FlowT where
    type StT FlowT a =
      StT (ExceptT FlowError) (StT (ReaderT FlowEnv) a)

    liftWith f = FlowT $
      liftWith $ \g ->
        liftWith $ \h ->
          liftWith $ \i ->
            f (i . h . g . unFlowT)

    restoreT = FlowT . restoreT . restoreT . restoreT

instance Monad m => MonadReader FlowEnv (FlowT m) where
  ask = FlowT ask
  local f = FlowT . local f . unFlowT

-- DecideT

runDecideT :: MonadIO m => DecideEnv -> DecideT m a -> m (Either FlowError a)
runDecideT e (DecideT k) = runExceptT (runReaderT (runLoggingT k l) e) where
  l = const . const . const $ deLogger e

instance MonadBase b m => MonadBase b (DecideT m) where
    liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (DecideT m) where
    type StM (DecideT m) a = ComposeSt DecideT m a

    liftBaseWith = defaultLiftBaseWith

    restoreM = defaultRestoreM

instance MonadTrans DecideT where
    lift = DecideT . lift . lift . lift

instance MonadTransControl DecideT where
    type StT DecideT a =
      StT (ExceptT FlowError) (StT (ReaderT DecideEnv) a)

    liftWith f = DecideT $
      liftWith $ \g ->
        liftWith $ \h ->
          liftWith $ \i ->
            f (i . h . g . unDecideT)

    restoreT = DecideT . restoreT . restoreT . restoreT

instance Monad m => MonadReader DecideEnv (DecideT m) where
  ask = DecideT ask
  local f = DecideT . local f . unDecideT

-- Planning

instance FromJSON Plan where
  parseJSON (Object v) =
    Plan           <$>
      v .: "start" <*>
      v .: "specs" <*>
      v .: "end"
  parseJSON _ = mzero

instance FromJSON Spec where
  parseJSON (Object v) =
    msum
      [ Work           <$>
          v .: "work"
      , Sleep          <$>
          v .: "sleep"
      ]
  parseJSON _ =
    mzero

instance FromJSON End where
  parseJSON (String v)
    | v == "stop"     = return Stop
    | v == "continue" = return Continue
    | otherwise = mzero
  parseJSON _ = mzero

instance FromJSON Start where
  parseJSON (Object v) =
    Start         <$>
      v .: "flow"
  parseJSON _ = mzero

instance FromJSON Task where
  parseJSON (Object v) =
    Task             <$>
      v .: "name"    <*>
      v .: "version" <*>
      v .: "queue"   <*>
      v .: "timeout"
  parseJSON _ = mzero

instance FromJSON Timer where
  parseJSON (Object v) =
    Timer            <$>
      v .: "name"    <*>
      v .: "timeout"
  parseJSON _ = mzero

-- Helpers

hoistAWSEither :: MonadError FlowError m => Either Error a -> m a
hoistAWSEither = either (throwError . AWSError) return

runAWS :: MonadFlow m => (FlowEnv -> Env) -> AWST m a -> m a
runAWS env action = do
  e <- asks env
  r <- runAWST e action
  hoistAWSEither r

throwStringError :: MonadError FlowError m => String -> m a
throwStringError = throwError . FlowError

hoistStringEither :: MonadError FlowError m => Either String a -> m a
hoistStringEither = either throwStringError return

runDecide :: (MonadError FlowError m, MonadIO m)
          => (LogStr -> IO ()) -> Uid -> Plan -> [HistoryEvent] -> DecideT m a -> m a
runDecide logger uid plan events action =
  runDecideT env action >>= hoistFlowEither
  where
    env = DecideEnv logger uid plan events findEvent where
      findEvent =
        flip lookup $ fromList $ flip map events $ \e ->
          (e ^. heEventId, e)
    hoistFlowEither = either throwError return

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

maybeToFlowError :: MonadError FlowError m => String -> Maybe a -> m a
maybeToFlowError e = hoistStringEither . maybeToEither e

-- Actions

registerDomainAction :: MonadFlow m => Domain -> m ()
registerDomainAction domain =
  runAWS feEnv $
    send_ $ registerDomain domain "30"

registerActivityTypeAction :: MonadFlow m => Domain -> Name -> Version -> Timeout -> m ()
registerActivityTypeAction domain name version timeout =
  runAWS feEnv $
    send_ $ registerActivityType domain name version &
      ratDefaultTaskHeartbeatTimeout .~ Just "NONE" &
      ratDefaultTaskScheduleToCloseTimeout .~ Just "NONE" &
      ratDefaultTaskScheduleToStartTimeout .~ Just "60" &
      ratDefaultTaskStartToCloseTimeout .~ Just timeout

registerWorkflowTypeAction :: MonadFlow m => Domain -> Name -> Version -> Timeout -> m ()
registerWorkflowTypeAction domain name version timeout =
  runAWS feEnv $
    send_ $ registerWorkflowType domain name version &
      rwtDefaultChildPolicy .~ Just Terminate &
      rwtDefaultExecutionStartToCloseTimeout .~ Just timeout &
      rwtDefaultTaskStartToCloseTimeout .~ Just "60"

startWorkflowExecutionAction :: MonadFlow m
                             => Domain -> Uid -> Name -> Version -> Queue -> Metadata -> m ()
startWorkflowExecutionAction domain uid name version queue input =
  runAWS feEnv $
    send_ $ startWorkflowExecution domain uid (workflowType name version) &
      swe1TaskList .~ Just (taskList queue) &
      swe1Input .~ input

pollForActivityTaskAction :: MonadFlow m => Domain -> Uid -> Queue -> m (Token, Metadata)
pollForActivityTaskAction domain uid queue =
  runAWS fePollEnv $ do
    r <- send $ pollForActivityTask domain (taskList queue) &
      pfatIdentity .~ Just uid
    return
      ( r ^. pfatrTaskToken
      , r ^. pfatrInput )

respondActivityTaskCompletedAction :: MonadFlow m => Token -> Metadata -> m ()
respondActivityTaskCompletedAction token result =
  runAWS feEnv $
    send_ $ respondActivityTaskCompleted token &
      ratcResult .~ result

respondActivityTaskFailedAction :: MonadFlow m => Token -> m ()
respondActivityTaskFailedAction token =
  runAWS feEnv $
    send_ $ respondActivityTaskFailed token

pollForDecisionTaskAction :: MonadFlow m
                          => Domain -> Uid -> Queue -> m (Maybe Token, [HistoryEvent])
pollForDecisionTaskAction domain uid queue =
  runAWS fePollEnv $ do
    rs <- paginate (pollForDecisionTask domain (taskList queue) &
      pfdtIdentity .~ Just uid &
      pfdtReverseOrder .~ Just True &
      pfdtMaximumPageSize .~ Just 100)
        $$ consume
    return
      ( liftM (^. pfdtrTaskToken) (headMay rs)
      , concatMap (^. pfdtrEvents) rs)

respondDecisionTaskCompletedAction :: MonadFlow m => Token -> [Decision] -> m ()
respondDecisionTaskCompletedAction token decisions =
  runAWS feEnv $
    send_ $ respondDecisionTaskCompleted token &
      rdtcDecisions .~ decisions

-- Decisions

scheduleActivityTaskDecision :: Uid -> Name -> Version -> Queue -> Metadata -> Decision
scheduleActivityTaskDecision uid name version list input =
  decision ScheduleActivityTask &
    dScheduleActivityTaskDecisionAttributes .~ Just attrs where
      attrs = scheduleActivityTaskDecisionAttributes (activityType name version) uid &
        satdaTaskList .~ Just (taskList list) &
        satdaInput .~ input

completeWorkflowExecutionDecision :: Metadata -> Decision
completeWorkflowExecutionDecision result =
  decision CompleteWorkflowExecution &
    dCompleteWorkflowExecutionDecisionAttributes .~ Just attrs where
      attrs = completeWorkflowExecutionDecisionAttributes &
        cwedaResult .~ result

startTimerDecision :: Uid -> Name -> Timeout -> Decision
startTimerDecision uid name timeout =
  decision StartTimer &
    dStartTimerDecisionAttributes .~ Just attrs where
      attrs = startTimerDecisionAttributes uid timeout &
        stdaControl .~ Just name

continueAsNewWorkflowExecutionDecision :: Version -> Queue -> Metadata -> Decision
continueAsNewWorkflowExecutionDecision version queue input =
  decision ContinueAsNewWorkflowExecution &
    dContinueAsNewWorkflowExecutionDecisionAttributes .~ Just attrs where
      attrs = continueAsNewWorkflowExecutionDecisionAttributes &
        canwedaWorkflowTypeVersion .~ Just version &
        canwedaTaskList .~ Just (taskList queue) &
        canwedaInput .~ input
