{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}

module Network.AWS.SWF.Flow.Internal
  ( runFlowT
  , runChoiceT
  , registerDomainAction
  , registerActivityTypeAction
  , registerWorkflowTypeAction
  , startWorkflowExecutionAction
  , pollForActivityTaskAction
  , respondActivityTaskCompletedAction
  , pollForDecisionTaskAction
  , respondDecisionTaskCompletedAction
  , scheduleActivityTaskDecision
  , completeWorkflowExecutionDecision
  , startTimerDecision
  , continueAsNewWorkflowExecutionDecision
  ) where

import Control.Lens                ( (^.), (.~), (&) )
import Control.Monad.Base          ( MonadBase, liftBase, liftBaseDefault )
import Control.Monad.Except        ( MonadError, ExceptT, runExceptT, throwError )
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
import Data.Conduit                ( ($$) )
import Data.Conduit.List           ( consume )
import Data.Maybe                  ( listToMaybe )
import Network.AWS.SWF
import Network.AWS.SWF.Flow.Types

-- FlowT

instance MonadBase b m => MonadBase b (FlowT m) where
    liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (FlowT m) where
    type StM (FlowT m) a = ComposeSt FlowT m a

    liftBaseWith = defaultLiftBaseWith

    restoreM = defaultRestoreM

instance MonadTrans FlowT where
    lift = FlowT . lift . lift

instance MonadTransControl FlowT where
    type StT FlowT a =
      StT (ExceptT FlowError) (StT (ReaderT Context) a)

    liftWith f = FlowT $
      liftWith $ \g ->
        liftWith $ \h ->
          f (h . g . unFlowT)

    restoreT = FlowT . restoreT . restoreT

instance Monad m => MonadReader Context (FlowT m) where
  ask = FlowT ask
  local f = FlowT . local f . unFlowT

-- ChoiceT

instance MonadBase b m => MonadBase b (ChoiceT m) where
    liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (ChoiceT m) where
    type StM (ChoiceT m) a = ComposeSt ChoiceT m a

    liftBaseWith = defaultLiftBaseWith

    restoreM = defaultRestoreM

instance MonadTrans ChoiceT where
    lift = ChoiceT . lift . lift

instance MonadTransControl ChoiceT where
    type StT ChoiceT a =
      StT (ExceptT FlowError) (StT (ReaderT Store) a)

    liftWith f = ChoiceT $
      liftWith $ \g ->
        liftWith $ \h ->
          f (h . g . unChoiceT)

    restoreT = ChoiceT . restoreT . restoreT

instance Monad m => MonadReader Store (ChoiceT m) where
  ask = ChoiceT ask
  local f = ChoiceT . local f . unChoiceT

-- Helpers

runFlowT :: Context -> FlowT m a -> m (Either FlowError a)
runFlowT c (FlowT k) = (runExceptT . runReaderT k) c

runChoiceT :: Store -> ChoiceT m a -> m (Either FlowError a)
runChoiceT c (ChoiceT k) = (runExceptT . runReaderT k) c

hoistAWSEither :: MonadError FlowError m => Either Error a -> m a
hoistAWSEither = either (throwError . AWSError) return

runAWST' :: MonadFlow m => (Context -> Env) -> AWST m a -> m a
runAWST' env action = do
  e <- asks env
  r <- runAWST e $ action
  hoistAWSEither r

-- Actions

registerDomainAction :: MonadFlow m => Domain -> m ()
registerDomainAction domain =
  runAWST' ctxEnv $
    send_ $ registerDomain domain "30"

registerActivityTypeAction :: MonadFlow m => Domain -> Name -> Version -> m ()
registerActivityTypeAction domain name version =
  runAWST' ctxEnv $
    send_ $ registerActivityType domain name version

registerWorkflowTypeAction :: MonadFlow m => Domain -> Name -> Version -> m ()
registerWorkflowTypeAction domain name version =
  runAWST' ctxEnv $
    send_ $ registerWorkflowType domain name version

startWorkflowExecutionAction :: MonadFlow m => Domain -> Uid -> Name -> Version -> Queue -> Metadata -> m ()
startWorkflowExecutionAction domain uid name version queue input =
  runAWST' ctxEnv $
    send_ $ startWorkflowExecution domain uid (workflowType name version) &
      swe1TaskList .~ Just (taskList queue) &
      swe1Input .~ input

pollForActivityTaskAction :: MonadFlow m => Domain -> Uid -> Queue -> m (Token, Metadata)
pollForActivityTaskAction domain uid queue =
  runAWST' ctxPollEnv $ do
    r <- send $ pollForActivityTask domain (taskList queue) &
      pfatIdentity .~ Just uid
    return $
      ( r ^. pfatrTaskToken
      , r ^. pfatrInput )

respondActivityTaskCompletedAction :: MonadFlow m => Token -> Metadata -> m ()
respondActivityTaskCompletedAction token result =
  runAWST' ctxEnv $
    send_ $ respondActivityTaskCompleted token &
      ratcResult .~ result

pollForDecisionTaskAction :: MonadFlow m => Domain -> Uid -> Queue -> m (Maybe Token, [HistoryEvent])
pollForDecisionTaskAction domain uid queue =
  runAWST' ctxPollEnv $ do
    rs <- paginate (pollForDecisionTask domain (taskList queue) &
      pfdtIdentity .~ Just uid &
      pfdtReverseOrder .~ Just True &
      pfdtMaximumPageSize .~ Just 100)
        $$ consume
    return $
      ( listToMaybe rs >>= return . (^. pfdtrTaskToken)
      , concatMap (^. pfdtrEvents) rs)

respondDecisionTaskCompletedAction :: MonadFlow m => Token -> [Decision] -> m ()
respondDecisionTaskCompletedAction token decisions =
  runAWST' ctxEnv $
    send_ $ respondDecisionTaskCompleted token &
      rdtcDecisions .~ decisions

-- Decisions

scheduleActivityTaskDecision :: Uid -> Name -> Version -> Queue -> Metadata -> Decision
scheduleActivityTaskDecision uid name version list input = do
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
