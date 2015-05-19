{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.AWS.SWF.Flow
  ( register
  , execute
  , act
  , decide
  , runFlowT
  , Domain
  , Uid
  , Name
  , Version
  , Queue
  , Token
  , Timeout
  , Metadata
  , FlowEnv (..)
  , FlowError
  , FlowT
  , MonadFlow
  , Task
  , Timer
  , Start
  , Spec (..)
  , End (..)
  , Plan
  ) where

import Control.Lens                  ( (^.) )
import Control.Monad                 ( foldM )
import Control.Monad.Except          ( throwError )
import Control.Monad.Reader          ( asks )
import Data.List                     ( find )
import Network.AWS.SWF
import Network.AWS.SWF.Flow.Internal
import Network.AWS.SWF.Flow.Types
import Safe                          ( headMay, tailMay )

-- Interface

register :: MonadFlow m => Domain -> Plan -> m [()]
register domain Plan{..} = do
  r <- registerDomainAction domain
  s <- registerWorkflowTypeAction domain
         (tskName $ strtTask plnStart)
         (tskVersion $ strtTask plnStart)
  foldM go [s, r] plnSpecs where
    go rs Work{..} = do
      r <- registerActivityTypeAction domain
             (tskName wrkTask)
             (tskVersion wrkTask)
      return (r : rs)
    go rs Sleep{..} = return rs

execute :: MonadFlow m => Domain -> Uid -> Task -> Metadata -> m ()
execute domain uid Task{..} input =
  startWorkflowExecutionAction domain uid tskName tskVersion tskQueue input

act :: MonadFlow m => Domain -> Uid -> Task -> (Metadata -> m Metadata) -> m ()
act domain uid Task{..} action = do
  (taskToken, input) <- pollForActivityTaskAction domain uid tskQueue
  output <- action input
  respondActivityTaskCompletedAction taskToken output

decide :: MonadFlow m => Domain -> Uid -> Plan -> m ()
decide domain uid plan@Plan{..} = do
   (token', events) <- pollForDecisionTaskAction domain uid (tskQueue $ strtTask plnStart)
   token <- maybeFlowError (FlowError "No Token") token'
   logger <- asks feLogger
   decisions <- runDecide logger uid plan events select
   respondDecisionTaskCompletedAction token decisions

-- Helpers

nextEvent :: MonadDecide m => [EventType] -> m HistoryEvent
nextEvent ets = do
  events <- asks deEvents
  maybeFlowError (FlowError "No Next Event") $ (flip find) events $ \e ->
    elem (e ^. heEventType) ets

workNext :: MonadDecide m => Name -> m (Maybe Spec)
workNext name = do
  specs <- asks (plnSpecs . dePlan)
  return $ tailMay (dropWhile p specs) >>= headMay where
    p Work{..} = tskName wrkTask /= name
    p _ = True

sleepNext :: MonadDecide m => Name -> m (Maybe Spec)
sleepNext name = do
  specs <- asks (plnSpecs . dePlan)
  return $ tailMay (dropWhile p specs) >>= headMay where
    p Sleep{..} = tmrName slpTimer /= name
    p _ = True

select :: MonadDecide m => m [Decision]
select = do
  event <- nextEvent [WorkflowExecutionStarted, ActivityTaskCompleted, TimerFired]
  case event ^. heEventType of
    WorkflowExecutionStarted -> start event
    ActivityTaskCompleted    -> completed event
    TimerFired               -> timer event
    _                        -> throwError (FlowError "Unknown Select Event")

start :: MonadDecide m => HistoryEvent -> m [Decision]
start event = do
  input <- maybeFlowError (FlowError "No Start Information") $ do
    attrs <- event ^. heWorkflowExecutionStartedEventAttributes
    return $ attrs ^. weseaInput
  specs <- asks (plnSpecs . dePlan)
  schedule input $ headMay specs

completed :: MonadDecide m => HistoryEvent -> m [Decision]
completed event = do
  findEvent <- asks deFindEvent
  (input, name) <- maybeFlowError (FlowError "No Completed Information") $ do
    attrs <- event ^. heActivityTaskCompletedEventAttributes
    event' <- findEvent $ attrs ^. atceaScheduledEventId
    attrs' <- event' ^. heActivityTaskScheduledEventAttributes
    return (attrs ^. atceaResult, attrs' ^. atseaActivityType ^. atName)
  next <- workNext name
  schedule input next

timer :: MonadDecide m => HistoryEvent -> m [Decision]
timer event = do
  findEvent <- asks deFindEvent
  name <- maybeFlowError (FlowError "No Timer Information") $ do
    attrs <- event ^. heTimerFiredEventAttributes
    event' <- findEvent $ attrs ^. tfeaStartedEventId
    attrs' <- event' ^. heTimerStartedEventAttributes
    attrs' ^. tseaControl
  event' <- nextEvent [WorkflowExecutionStarted, ActivityTaskCompleted]
  case event' ^. heEventType of
    WorkflowExecutionStarted -> timerStart event' name
    ActivityTaskCompleted    -> timerCompleted event' name
    _                        -> throwError (FlowError "Unknown Timer Event")

timerStart :: MonadDecide m => HistoryEvent -> Name -> m [Decision]
timerStart event name = do
  input <- maybeFlowError (FlowError "No Timer Start Information") $ do
    attrs <- event ^. heWorkflowExecutionStartedEventAttributes
    return $ attrs ^. weseaInput
  next <- sleepNext name
  schedule input next

timerCompleted :: MonadDecide m => HistoryEvent -> Name -> m [Decision]
timerCompleted event name = do
  input <- maybeFlowError (FlowError "No Timer Completed Information") $ do
    attrs <- event ^. heActivityTaskCompletedEventAttributes
    return $ attrs ^. atceaResult
  next <- sleepNext name
  schedule input next

schedule :: MonadDecide m => Metadata -> Maybe Spec -> m [Decision]
schedule input = maybe (scheduleEnd input) (scheduleSpec input)

scheduleSpec :: MonadDecide m => Metadata -> Spec -> m [Decision]
scheduleSpec input spec = do
  uid <- asks deUid
  case spec of
    Work{..} ->
      return [scheduleActivityTaskDecision uid
               (tskName wrkTask)
               (tskVersion wrkTask)
               (tskQueue wrkTask)
               input]
    Sleep{..} ->
      return [startTimerDecision uid
               (tmrTimeout slpTimer)
               (tmrName slpTimer)]

scheduleEnd :: MonadDecide m => Metadata -> m [Decision]
scheduleEnd input = do
  end <- asks (plnEnd . dePlan)
  case end of
    Stop -> return [completeWorkflowExecutionDecision input]
    Continue -> scheduleContinue

scheduleContinue :: MonadDecide m => m [Decision]
scheduleContinue = do
  event <- nextEvent [WorkflowExecutionStarted]
  input <- maybeFlowError (FlowError "No Continue Start Information") $ do
    attrs <- event ^. heWorkflowExecutionStartedEventAttributes
    return $ attrs ^. weseaInput
  task <- asks (strtTask . plnStart . dePlan)
  return [continueAsNewWorkflowExecutionDecision
           (tskVersion task)
           (tskQueue task)
           input]
