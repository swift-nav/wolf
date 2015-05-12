{-# LANGUAGE RecordWildCards #-}

module Network.AWS.SWF.Flow
  ( defaultConfig
  , execute
  , act
  , decide
  ) where

import Control.Lens                         ( (^.) )
import Control.Monad.Trans.Either           ( EitherT(..), hoistEither, left )
import Data.HashMap.Strict                  ( fromList, lookup )
import Data.List                            ( find )
import Data.Text                            ( Text )
import Prelude                       hiding ( lookup )
import Network.AWS.SWF
import Network.AWS.SWF.Flow.Types
import Network.AWS.SWF.Flow.Internal

-- Helpers

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e a = maybe (Left e) Right a

hoistMaybeToEither :: e -> Maybe a -> EitherT e IO a
hoistMaybeToEither e = hoistEither . maybeToEither e

-- Interface

execute :: Config -> Text -> Task -> Maybe Text -> IO (EitherE ())
execute config domain Task {..} input =
  withContext config $ \Context {..} ->
    runStartWorkflowExecution ctxEnv domain ctxUid tskName tskVersion tskList input

act :: Config -> Text -> Task -> (Maybe Text -> IO (EitherE (Maybe Text))) -> IO (EitherE ())
act config domain Task {..} worker =
  withContext config $ \Context {..} ->
    runEitherT $ do
      (taskToken, input) <- EitherT $ runPollForActivityTask ctxPollEnv domain ctxUid tskList
      result <- EitherT $ worker input
      EitherT $ runRespondActivityTaskCompleted ctxEnv taskToken result

decide :: Config -> Text -> Spec -> IO (EitherE ())
decide config domain spec =
  withContext config $ \Context {..} ->
    let
      nextEventType es =
        find $ \event ->
          elem (event ^. heEventType) es

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

      schedule taskToken input =
        go where
          go Work {..} =
            EitherT $ runRespondDecisionTaskCompleted ctxEnv taskToken
              [scheduleActivityTask ctxUid
                (tskName wrkTask)
                (tskVersion wrkTask)
                (tskList wrkTask)
                input]
          go Sleep {..} =
            EitherT $ runRespondDecisionTaskCompleted ctxEnv taskToken
              [startTimer ctxUid (tmrTimeout slpTimer) (tmrName slpTimer)]
          go Continue = return ()
          go Done = return ()
          go _ = left (error "Bad Schedule Spec")
    in
    runEitherT $
      case spec of
        Start {..} -> do
          (taskToken', events) <- EitherT $ runPollForDecisionTask ctxPollEnv domain ctxUid (tskList strtTask)
          taskToken <- hoistMaybeToEither (error "No Task Token") taskToken'
          let eventMap = fromList $ (flip map) events $ \e -> (e ^. heEventId, e)
          event <- hoistMaybeToEither (error "No Events") $
            nextEventType [WorkflowExecutionStarted, ActivityTaskCompleted, TimerFired] events
          case event ^. heEventType of
            WorkflowExecutionStarted -> do
              attrs <- hoistMaybeToEither (error "No Attributes") $
                event ^. heWorkflowExecutionStartedEventAttributes
              schedule taskToken (attrs ^. weseaInput) strtNext
            ActivityTaskCompleted -> do
              attrs <- hoistMaybeToEither (error "No Attributes") $
                event ^. heActivityTaskCompletedEventAttributes
              nextSpec <- hoistMaybeToEither (error "No Completed Spec") $ do
                event' <- lookup (attrs ^. atceaScheduledEventId) eventMap
                attrs' <- event' ^. heActivityTaskScheduledEventAttributes
                findWork (attrs' ^. atseaActivityType ^. atName)
              schedule taskToken (attrs ^. atceaResult) nextSpec
            TimerFired -> do
              attrs <- hoistMaybeToEither (error "No Attributes") $
                event ^. heTimerFiredEventAttributes
              nextSpec <- hoistMaybeToEither (error "No Timer Spec") $ do
                event' <- lookup (attrs ^. tfeaStartedEventId) eventMap
                attrs' <- event' ^. heTimerStartedEventAttributes
                name <- (attrs' ^. tseaControl)
                findSleep name
              event' <- hoistMaybeToEither (error "No Events") $
                nextEventType [WorkflowExecutionStarted, ActivityTaskCompleted] events
              case event' ^. heEventType of
                WorkflowExecutionStarted -> do
                  attrs'' <- hoistMaybeToEither (error "No Attributes") $
                    event' ^. heWorkflowExecutionStartedEventAttributes
                  schedule taskToken (attrs'' ^. weseaInput) nextSpec
                ActivityTaskCompleted -> do
                  attrs'' <- hoistMaybeToEither (error "No Attributes") $
                    event' ^. heActivityTaskCompletedEventAttributes
                  schedule taskToken (attrs'' ^. atceaResult) nextSpec
                _ ->
                  left (error "Unknown Event")
            _ ->
              left (error "Unknown Event")
        _ ->
          left (error "No Start Spec")
