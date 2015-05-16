{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.AWS.SWF.Flow
  ( register
  , execute
  , act
  , decide
  ) where

import Control.Lens                         ( (^.) )
import Control.Monad.Reader                 ( asks )
import Control.Monad.Except                 ( MonadError, throwError )
import Data.List                            ( find )
import Data.HashMap.Strict                  ( fromList, lookup )
import Network.AWS.SWF
import Network.AWS.SWF.Flow.Internal
import Network.AWS.SWF.Flow.Types
import Prelude                       hiding ( lookup )

-- Helpers

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e a = maybe (Left e) Right a

hoistFlowEither :: MonadError FlowError m => Either FlowError a -> m a
hoistFlowEither = either throwError return

maybeFlowError :: MonadError FlowError m => FlowError -> Maybe a -> m a
maybeFlowError e = hoistFlowEither . maybeToEither e

-- Interface

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

execute :: MonadFlow m => Domain -> Task -> Metadata -> m ()
execute domain Task{..} input = do
  uid <- asks ctxUid
  startWorkflowExecutionAction domain uid tskName tskVersion tskQueue input

act :: MonadFlow m => Domain -> Task -> (Metadata -> m Metadata) -> m ()
act domain Task{..} action = do
  uid <- asks ctxUid
  (taskToken, input) <- pollForActivityTaskAction domain uid tskQueue
  output <- action input
  respondActivityTaskCompletedAction taskToken output

decide :: MonadFlow m => Domain -> Spec -> m ()
decide domain spec = do
  uid <- asks ctxUid
  (token', events) <- pollForDecisionTaskAction domain uid (tskQueue (strtTask spec))
  token <- maybeFlowError (FlowError "No Token") token'
  decisions <- runChoose (store uid spec events) choose
  respondDecisionTaskCompletedAction token decisions

---------------------

store :: Uid -> Spec -> [HistoryEvent] -> Store
store uid spec events = Store uid spec events $
  (flip lookup) $ fromList $ (flip map) events $ \e -> (e ^. heEventId, e)

nextEvent :: MonadChoice m => [EventType] -> m HistoryEvent
nextEvent ets = do
  events <- asks strEvents
  maybeFlowError (FlowError "No Next Event") $
    (flip find) events $ \e ->
      elem (e ^. heEventType) ets

workNext :: MonadChoice m => Name -> m Spec
workNext name = do
  let go Work{..}
        | tskName wrkTask == name = return wrkNext
        | otherwise = go wrkNext
      go Start{..} = go strtNext
      go Sleep{..} = go slpNext
      go _ = throwError (FlowError "No Work Next Spec")
  spec <- asks strSpec
  go spec

sleepNext :: MonadChoice m => Name -> m Spec
sleepNext name = do
  let go Sleep {..}
        | tmrName slpTimer == name = return slpNext
        | otherwise = go slpNext
      go Start {..} = go strtNext
      go Work {..} = go wrkNext
      go _ = throwError (FlowError "No Sleep Next Spec")
  spec <- asks strSpec
  go spec

scheduleContinue :: MonadChoice m => m [Decision]
scheduleContinue = do
  task <- asks (strtTask . strSpec)
  event <- nextEvent [WorkflowExecutionStarted]
  input <- maybeFlowError (FlowError "No Continue Start Information") $ do
    attrs <- event ^. heWorkflowExecutionStartedEventAttributes
    return $ attrs ^. weseaInput
  return [continueAsNewWorkflowExecutionDecision
           (tskVersion task)
           (tskQueue task)
           input]

schedule :: MonadChoice m => Spec -> Metadata -> m [Decision]
schedule spec input = do
  uid <- asks strUid
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

start :: MonadChoice m => HistoryEvent -> m [Decision]
start event = do
  input <- maybeFlowError (FlowError "No Start Information") $ do
    attrs <- event ^. heWorkflowExecutionStartedEventAttributes
    return $ attrs ^. weseaInput
  next <- asks (strtNext . strSpec)
  schedule next input

completed :: MonadChoice m => HistoryEvent -> m [Decision]
completed event = do
  findEvent <- asks strFindEvent
  (input, name) <- maybeFlowError (FlowError "No Completed Information") $ do
    attrs <- event ^. heActivityTaskCompletedEventAttributes
    event' <- findEvent $ attrs ^. atceaScheduledEventId
    attrs' <- event' ^. heActivityTaskScheduledEventAttributes
    return (attrs ^. atceaResult, attrs' ^. atseaActivityType ^. atName)
  next <- workNext name
  schedule next input

timer :: MonadChoice m => HistoryEvent -> m [Decision]
timer event = do
  findEvent <- asks strFindEvent
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

timerStart :: MonadChoice m => HistoryEvent -> Spec -> m [Decision]
timerStart event next = do
  input <- maybeFlowError (FlowError "No Timer Start Information") $ do
    attrs <- event ^. heWorkflowExecutionStartedEventAttributes
    return $ attrs ^. weseaInput
  schedule next input

timerCompleted :: MonadChoice m => HistoryEvent -> Spec -> m [Decision]
timerCompleted event next = do
  input <- maybeFlowError (FlowError "No Timer Completed Information") $ do
    attrs <- event ^. heActivityTaskCompletedEventAttributes
    return $ attrs ^. atceaResult
  schedule next input

choose :: MonadChoice m => m [Decision]
choose = do
  event <- nextEvent [WorkflowExecutionStarted, ActivityTaskCompleted, TimerFired]
  case event ^. heEventType of
    WorkflowExecutionStarted -> start event
    ActivityTaskCompleted    -> completed event
    TimerFired               -> timer event
    _                        -> throwError (FlowError "Unknown Event")

runChoose :: MonadError FlowError m => Store -> ChoiceT m b -> m b
runChoose s action = do
  r <- runChoiceT s action
  hoistFlowEither r

