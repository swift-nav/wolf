{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Decider logic.
--
module Network.AWS.Wolf.Decide
  ( decide
  , decideMain
  ) where

import Data.Aeson
import Data.Time
import Data.UUID
import Data.UUID.V4
import Network.AWS.SWF
import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.File
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.SWF
import Network.AWS.Wolf.Types

-- | Successful end of workflow.
--
end :: MonadAmazonDecision c m => Maybe Text -> m Decision
end input = do
  traceInfo "end" mempty
  pure $ completeWork input

-- | Next activity in workflow to run.
--
next :: MonadAmazonDecision c m => Maybe Text -> Maybe Text -> Task -> m Decision
next input priority t = do
  uid <- liftIO $ toText <$> nextRandom
  traceInfo "next" [ "uid" .= uid, "task" .= t ]
  pure $ scheduleWork uid (t ^. tName) (t ^. tVersion) (t ^. tQueue) input priority

-- | Failed activity, stop the workflow.
--
failed :: MonadAmazonDecision c m => m Decision
failed = do
  traceInfo "failed" mempty
  pure failWork

-- | Completed activity, start the next activity.
--
completed :: MonadAmazonDecision c m => HistoryEvent -> m Decision
completed he = do
  traceInfo "completed" mempty
  hes <- view adcEvents
  (input, priority, name) <- maybeThrowIO' "No Completed Information" $ do
    atcea <- he ^. heActivityTaskCompletedEventAttributes
    he'   <- flip find hes $ (== atcea ^. atceaScheduledEventId) . view heEventId
    atsea <- he' ^. heActivityTaskScheduledEventAttributes
    pure (atcea ^. atceaResult, atsea ^. atseaTaskPriority, atsea ^. atseaActivityType . atName)
  p <- view adcPlan
  maybe (end input) (next input priority) $
    tailMay (flip dropWhile (p ^. pTasks) $ (/= name) . view tName) >>= headMay

-- | Beginning of workflow, start the first activity.
--
begin :: MonadAmazonDecision c m => HistoryEvent -> m Decision
begin he = do
  traceInfo "begin" mempty
  (input, priority) <- maybeThrowIO' "No Start Information" $ do
    wesea <- he ^. heWorkflowExecutionStartedEventAttributes
    pure (wesea ^. weseaInput, wesea ^. weseaTaskPriority)
  p <- view adcPlan
  maybe (end input) (next input priority) $ headMay (p ^. pTasks)

-- | Schedule workflow based on historical events.
--
schedule :: MonadAmazonDecision c m => m Decision
schedule = do
  traceInfo "schedule" mempty
  hes <- view adcEvents
  f hes >>=
    maybeThrowIO' "No Select Information"
  where
    f []       = pure Nothing
    f (he:hes) =
      case he ^. heEventType of
        WorkflowExecutionStarted -> Just <$> begin he
        ActivityTaskCompleted    -> Just <$> completed he
        ActivityTaskFailed       -> Just <$> failed
        _et                      -> f hes

-- | Decider logic - poll for decisions, make decisions.
--
decide :: MonadConf c m => Plan -> m ()
decide p =
  preConfCtx [ "label" .= LabelDecide ] $ do
    let queue = p ^. pStart . tQueue
    runAmazonWorkCtx queue $ do
      traceInfo "poll" mempty
      t0 <- liftIO getCurrentTime
      (token, hes) <- pollDecision
      t1 <- liftIO getCurrentTime
      statsIncrement "wolf.decide.poll.count" [ "queue" =. queue ]
      statsHistogram "wolf.decide.poll.elapsed" (realToFrac (diffUTCTime t1 t0) :: Double) [ "queue" =. queue ]
      maybe_ token $ \token' ->
        runAmazonDecisionCtx p hes $ do
          traceInfo "start" mempty
          t2 <- liftIO getCurrentTime
          schedule >>=
            completeDecision token'
          t3 <- liftIO getCurrentTime
          traceInfo "finish" mempty
          statsIncrement "wolf.decide.decision.count" [ "queue" =. queue ]
          statsHistogram "wolf.decide.decision.elapsed" (realToFrac (diffUTCTime t3 t2) :: Double) [ "queue" =. queue ]

-- | Run decider from main with config file.
--
decideMain :: MonadControl m => FilePath -> FilePath -> Maybe Text -> m ()
decideMain cf pf domain  =
  runCtx $ runTop $ do
    conf <- readYaml cf
    let conf' = override cDomain domain conf
    runConfCtx conf' $ do
      plans <- readYaml pf
      runConcurrent $
        forever . decide <$> plans
