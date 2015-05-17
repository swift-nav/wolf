{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.AWS.SWF.Flow
  ( defaultConfig
  , runFlow
  , register
  , execute
  , act
  , decide
  ) where

import Control.Lens                         ( (^.), (.~), (<&>) )
import Control.Monad.Reader                 ( asks )
import Control.Monad.Except                 ( throwError )
import Data.List                            ( find )
import Data.HashMap.Strict                  ( fromList, lookup )
import Control.Monad.Trans.AWS              ( Region(..)
                                            , Credentials(..)
                                            , LogLevel(..)
                                            , envLogger
                                            , newLogger
                                            , newEnv )
import Control.Monad.Trans.Except           ( runExceptT )
import Network.AWS.SWF
import Network.AWS.SWF.Flow.Internal
import Network.AWS.SWF.Flow.Types
import Network.HTTP.Conduit                 ( conduitManagerSettings )
import Network.HTTP.Client                  ( ManagerSettings(..), withManager )
import Prelude                       hiding ( lookup )
import System.IO                            ( stdout )

-- Interface

defaultConfig :: FlowConfig
defaultConfig = FlowConfig
  { fcRegion      = NorthVirginia
  , fcCredentials = FromEnv "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY"
  , fcTimeout     = 5000000
  , fcPollTimeout = 70000000
  , fcLogLevel    = Info
  , fcLogHandle   = stdout
  }

runFlow :: FlowConfig -> FlowT IO a -> IO (Either FlowError a)
runFlow FlowConfig{..} action = do
  let managerSettings timeout =
        conduitManagerSettings { managerResponseTimeout = Just timeout }
  let newEnv' manager =
        runExceptT (newEnv fcRegion fcCredentials manager) >>= either error return
  withManager (managerSettings fcTimeout) $ \manager ->
    withManager (managerSettings fcPollTimeout) $ \pollManager -> do
      logger <- newLogger fcLogLevel fcLogHandle
      env <- newEnv' manager <&> envLogger .~ logger
      pollEnv <- newEnv' pollManager <&> envLogger .~ logger
      runFlowT (FlowEnv env pollEnv) action

register :: MonadFlow m => Domain -> Spec -> m [()]
register domain spec = do
  let registerAll =
        go spec where
          go Start{..} = do
            r <- registerWorkflowTypeAction domain (tskName strtTask) (tskVersion strtTask)
            rs <- go strtNext
            return (r : rs)
          go Work{..} = do
            r <- registerActivityTypeAction domain (tskName wrkTask) (tskVersion wrkTask)
            rs <- go wrkNext
            return (r : rs)
          go Sleep{..} = go slpNext
          go _ = return []
  r <- registerDomainAction domain
  rs <- registerAll
  return (r : rs)

execute :: MonadFlow m => Domain -> Uid -> Task -> Metadata -> m ()
execute domain uid Task{..} input =
  startWorkflowExecutionAction domain uid tskName tskVersion tskQueue input

act :: MonadFlow m => Domain -> Uid -> Task -> (Metadata -> m Metadata) -> m ()
act domain uid Task{..} action = do
  (taskToken, input) <- pollForActivityTaskAction domain uid tskQueue
  output <- action input
  respondActivityTaskCompletedAction taskToken output

decide :: MonadFlow m => Domain -> Uid -> Spec -> m ()
decide domain uid spec = do
  (token', events) <- pollForDecisionTaskAction domain uid (tskQueue (strtTask spec))
  token <- maybeFlowError (FlowError "No Token") token'
  decisions <- runDecide (decideEnv uid spec events) select
  respondDecisionTaskCompletedAction token decisions

-- Helpers

decideEnv :: Uid -> Spec -> [HistoryEvent] -> DecideEnv
decideEnv uid spec events = DecideEnv uid spec events $
  (flip lookup) $ fromList $ (flip map) events $ \e ->
    (e ^. heEventId, e)

nextEvent :: MonadDecide m => [EventType] -> m HistoryEvent
nextEvent ets = do
  events <- asks deEvents
  maybeFlowError (FlowError "No Next Event") $
    (flip find) events $ \e ->
      elem (e ^. heEventType) ets

workNext :: MonadDecide m => Name -> m Spec
workNext name = do
  let go Work{..}
        | tskName wrkTask == name = return wrkNext
        | otherwise = go wrkNext
      go Start{..} = go strtNext
      go Sleep{..} = go slpNext
      go _ = throwError (FlowError "No Work Next Spec")
  spec <- asks deSpec
  go spec

sleepNext :: MonadDecide m => Name -> m Spec
sleepNext name = do
  let go Sleep {..}
        | tmrName slpTimer == name = return slpNext
        | otherwise = go slpNext
      go Start {..} = go strtNext
      go Work {..} = go wrkNext
      go _ = throwError (FlowError "No Sleep Next Spec")
  spec <- asks deSpec
  go spec

select :: MonadDecide m => m [Decision]
select = do
  event <- nextEvent [WorkflowExecutionStarted, ActivityTaskCompleted, TimerFired]
  case event ^. heEventType of
    WorkflowExecutionStarted -> start event
    ActivityTaskCompleted    -> completed event
    TimerFired               -> timer event
    _                        -> throwError (FlowError "Unknown Event")

start :: MonadDecide m => HistoryEvent -> m [Decision]
start event = do
  input <- maybeFlowError (FlowError "No Start Information") $ do
    attrs <- event ^. heWorkflowExecutionStartedEventAttributes
    return $ attrs ^. weseaInput
  next <- asks (strtNext . deSpec)
  schedule next input

completed :: MonadDecide m => HistoryEvent -> m [Decision]
completed event = do
  findEvent <- asks deFindEvent
  (input, name) <- maybeFlowError (FlowError "No Completed Information") $ do
    attrs <- event ^. heActivityTaskCompletedEventAttributes
    event' <- findEvent $ attrs ^. atceaScheduledEventId
    attrs' <- event' ^. heActivityTaskScheduledEventAttributes
    return (attrs ^. atceaResult, attrs' ^. atseaActivityType ^. atName)
  next <- workNext name
  schedule next input

timer :: MonadDecide m => HistoryEvent -> m [Decision]
timer event = do
  findEvent <- asks deFindEvent
  name <- maybeFlowError (FlowError "No Timer Information") $ do
    attrs <- event ^. heTimerFiredEventAttributes
    event' <- findEvent $ attrs ^. tfeaStartedEventId
    attrs' <- event' ^. heTimerStartedEventAttributes
    attrs' ^. tseaControl
  next <- sleepNext name
  event' <- nextEvent [WorkflowExecutionStarted, ActivityTaskCompleted]
  case event' ^. heEventType of
    WorkflowExecutionStarted -> timerStart event' next
    ActivityTaskCompleted    -> timerCompleted event' next
    _                        -> throwError (FlowError "Unknown Timer Event")

timerStart :: MonadDecide m => HistoryEvent -> Spec -> m [Decision]
timerStart event next = do
  input <- maybeFlowError (FlowError "No Timer Start Information") $ do
    attrs <- event ^. heWorkflowExecutionStartedEventAttributes
    return $ attrs ^. weseaInput
  schedule next input

timerCompleted :: MonadDecide m => HistoryEvent -> Spec -> m [Decision]
timerCompleted event next = do
  input <- maybeFlowError (FlowError "No Timer Completed Information") $ do
    attrs <- event ^. heActivityTaskCompletedEventAttributes
    return $ attrs ^. atceaResult
  schedule next input

schedule :: MonadDecide m => Spec -> Metadata -> m [Decision]
schedule spec input = do
  uid <- asks deUid
  let go Work{..} =
        return [scheduleActivityTaskDecision uid
                 (tskName wrkTask)
                 (tskVersion wrkTask)
                 (tskQueue wrkTask)
                 input]
      go Sleep{..} =
        return [startTimerDecision uid
                 (tmrTimeout slpTimer)
                 (tmrName slpTimer)]
      go Done =
        return [completeWorkflowExecutionDecision input]
      go Continue = scheduleContinue
      go _ = throwError (FlowError "Bad Schedule Spec")
  go spec

scheduleContinue :: MonadDecide m => m [Decision]
scheduleContinue = do
  task <- asks (strtTask . deSpec)
  event <- nextEvent [WorkflowExecutionStarted]
  input <- maybeFlowError (FlowError "No Continue Start Information") $ do
    attrs <- event ^. heWorkflowExecutionStartedEventAttributes
    return $ attrs ^. weseaInput
  return [continueAsNewWorkflowExecutionDecision
           (tskVersion task)
           (tskQueue task)
           input]
