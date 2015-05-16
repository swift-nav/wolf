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

import Control.Monad                        ( liftM2 )
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
register domain spec =
  let
    registerAll =
      go spec where
        go Start{..} =
          liftM2 (:) (registerWorkflowTypeAction domain
                        (tskName strtTask)
                        (tskVersion strtTask))
                     (go strtNext)
        go Work{..} =
          liftM2 (:) (registerActivityTypeAction domain
                        (tskName wrkTask)
                        (tskVersion wrkTask))
                     (go wrkNext)
        go Sleep{..} =
          go slpNext
        go _ =
          return []
  in
    liftM2 (:) (registerDomainAction domain) registerAll

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
decide domain spec =
  let
    findWork name =
      go spec where
        go Work {..}
          | tskName wrkTask == name = Just wrkNext
          | otherwise = go wrkNext
        go Start {..} = go strtNext
        go Sleep {..} = go slpNext
        go _ = Nothing

    findSleep name =
      go spec where
        go Sleep {..}
          | tmrName slpTimer == name = Just slpNext
          | otherwise = go slpNext
        go Start {..} = go strtNext
        go Work {..} = go wrkNext
        go _ = Nothing
  in
  case spec of
    Start{..} -> do
      uid <- asks ctxUid
      (token', events) <- pollForDecisionTaskAction domain uid (tskQueue strtTask)
      token <- maybeFlowError (FlowError "No Token") token'

      let eventMap =
            fromList $ (flip map) events $ \e -> (e ^. heEventId, e)

      let nextEvent es =
            (flip find) events $ \e -> elem (e ^. heEventType) es

      let schedule input =
            go where
              go Work{..} =
                respondDecisionTaskCompletedAction token
                  [scheduleActivityTaskDecision uid
                    (tskName wrkTask)
                    (tskVersion wrkTask)
                    (tskQueue wrkTask)
                    input]
              go Sleep {..} =
                respondDecisionTaskCompletedAction token
                  [startTimerDecision uid
                    (tmrTimeout slpTimer)
                    (tmrName slpTimer)]
              go Continue =
                respondDecisionTaskCompletedAction token []
              go Done =
                return ()
              go _ =
                throwError (FlowError "Bad Schedule Spec")

      let start event = do
            input <- maybeFlowError (FlowError "No Start Information") $ do
              attrs <- event ^. heWorkflowExecutionStartedEventAttributes
              return (attrs ^. weseaInput)
            schedule input strtNext

      let completed event = do
            (input, next) <- maybeFlowError (FlowError "No Completed Information") $ do
              attrs <- event ^. heActivityTaskCompletedEventAttributes
              event' <- lookup (attrs ^. atceaScheduledEventId) eventMap
              attrs' <- event' ^. heActivityTaskScheduledEventAttributes
              next <- findWork (attrs' ^. atseaActivityType ^. atName)
              return (attrs ^. atceaResult, next)
            schedule input next

      let timerStart event next = do
            input <- maybeFlowError (FlowError "No Timer Start Information") $ do
              attrs <- event ^. heWorkflowExecutionStartedEventAttributes
              return (attrs ^. weseaInput)
            schedule input next

      let timerCompleted event next = do
            input <- maybeFlowError (FlowError "No Timer Completed Information") $ do
              attrs <- event ^. heActivityTaskCompletedEventAttributes
              return (attrs ^. atceaResult)
            schedule input next

      let timer event = do
            (event', next) <- maybeFlowError (FlowError "No Timer Information") $ do
              event' <- nextEvent [WorkflowExecutionStarted, ActivityTaskCompleted]
              attrs <- event ^. heTimerFiredEventAttributes
              event'' <- lookup (attrs ^. tfeaStartedEventId) eventMap
              attrs'' <- event'' ^. heTimerStartedEventAttributes
              name <- (attrs'' ^. tseaControl)
              next <- findSleep name
              return (event', next)
            case event' ^. heEventType of
              WorkflowExecutionStarted -> timerStart event' next
              ActivityTaskCompleted    -> timerCompleted event' next
              _                        -> throwError (FlowError "Unknown Timer Event")

      event <- maybeFlowError (FlowError "No Next Event") $
        nextEvent [WorkflowExecutionStarted, ActivityTaskCompleted, TimerFired]
      case event ^. heEventType of
        WorkflowExecutionStarted -> start event
        ActivityTaskCompleted    -> completed event
        TimerFired               -> timer event
        _                        -> throwError (FlowError "Unknown Event")
    _ -> throwError (FlowError "No Start Spec")
