{-# LANGUAGE RecordWildCards #-}

module Network.AWS.SWF.Flow
  ( defaultConfig
  , execute
  , act
  , decide
  ) where

import Control.Lens                         ( (^.) )
import Control.Monad.IO.Class               ( liftIO )
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
    in
    runEitherT $
      case spec of
        Start {..} -> do
          (taskToken', events) <- EitherT $ runPollForDecisionTask ctxPollEnv domain ctxUid (tskList strtTask)
          taskToken <- hoistMaybeToEither (error "No Task Token") taskToken'

          let eventMap =
                fromList $ (flip map) events $ \e -> (e ^. heEventId, e)

          let schedule input =
                go where
                  go Work {..} = do
                    liftIO $ putStrLn $ "Scheduling Work " ++ show (tskName wrkTask)
                    EitherT $ runRespondDecisionTaskCompleted ctxEnv taskToken
                      [scheduleActivityTask ctxUid
                        (tskName wrkTask)
                        (tskVersion wrkTask)
                        (tskList wrkTask)
                        input]
                  go Sleep {..} = do
                    liftIO $ putStrLn $ "Scheduling Sleep " ++ show (tmrName slpTimer)
                    EitherT $ runRespondDecisionTaskCompleted ctxEnv taskToken
                      [startTimer ctxUid (tmrTimeout slpTimer) (tmrName slpTimer)]
                  go Continue = do
                    liftIO $ putStrLn $ "Continuing " ++ show (tskName strtTask)
                    attrs <- hoistMaybeToEither (error "No Events") $ do
                      event <- nextEventType [WorkflowExecutionStarted] events
                      event ^. heWorkflowExecutionStartedEventAttributes
                    EitherT $ runRespondDecisionTaskCompleted ctxEnv taskToken
                      [continueAsNewWorkflowExecution
                        (tskVersion strtTask)
                        (tskList strtTask)
                        (attrs ^. weseaInput)]
                  go Done = do
                    liftIO $ putStrLn $ "Done " ++ show input
                    return ()
                  go _ =
                    left (error "Bad Schedule Spec")

          event <- hoistMaybeToEither (error "No Events") $
            nextEventType [WorkflowExecutionStarted, ActivityTaskCompleted, TimerFired] events
          case event ^. heEventType of
            WorkflowExecutionStarted -> do
              liftIO $ putStrLn $ "Got WorkflowExecutionStarted Event"
              attrs <- hoistMaybeToEither (error "No Attributes") $
                event ^. heWorkflowExecutionStartedEventAttributes
              schedule (attrs ^. weseaInput) strtNext
            ActivityTaskCompleted -> do
              liftIO $ putStrLn $ "Got ActivityTaskCompleted Event"
              attrs <- hoistMaybeToEither (error "No Attributes") $
                event ^. heActivityTaskCompletedEventAttributes
              nextSpec <- hoistMaybeToEither (error "No Completed Spec") $ do
                event' <- lookup (attrs ^. atceaScheduledEventId) eventMap
                attrs' <- event' ^. heActivityTaskScheduledEventAttributes
                findWork (attrs' ^. atseaActivityType ^. atName)
              schedule (attrs ^. atceaResult) nextSpec
            TimerFired -> do
              liftIO $ putStrLn $ "Got TimerFired Event"
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
                  liftIO $ putStrLn $ "Awake From WorkflowExecutionStarted Event"
                  attrs'' <- hoistMaybeToEither (error "No Attributes") $
                    event' ^. heWorkflowExecutionStartedEventAttributes
                  schedule (attrs'' ^. weseaInput) nextSpec
                ActivityTaskCompleted -> do
                  liftIO $ putStrLn $ "Awake From ActivityTaskCompleted Event"
                  attrs'' <- hoistMaybeToEither (error "No Attributes") $
                    event' ^. heActivityTaskCompletedEventAttributes
                  schedule (attrs'' ^. atceaResult) nextSpec
                _ ->
                  left (error "Unknown Event")
            _ ->
              left (error "Unknown Event")
        _ ->
          left (error "No Start Spec")
