{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Counter logic.
--
module Network.AWS.Wolf.Count
  ( count
  , countMain
  ) where

import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.File
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.SWF
import Network.AWS.Wolf.Types

-- | Count pending activities.
--
countActivity :: MonadConf c m => Task -> m ()
countActivity t = do
  traceInfo "count-act" [ "task" .= t ]
  let queue = t ^. tQueue
  runAmazonWorkCtx queue $ do
    c <- countActivities
    traceInfo "count-activities" [ "task" .= t, "count" .= c ]
    statsGauge "wolf.act.queue.depth" c [ "queue" =. queue ]

-- | Count open workflows.
--
countDecision :: MonadConf c m => Task -> m ()
countDecision t = do
  traceInfo "count-decision" [ "task" .= t ]
  let queue = t ^. tQueue
  runAmazonWorkCtx queue $ do
    c <- countDecisions
    traceInfo "count-decisions" [ "task" .= t, "count" .= c ]
    statsGauge "wolf.decide.queue.depth" c [ "queue" =. queue ]

-- | Counter logic - count all the queues.
--
count :: MonadConf c m => Plan -> m ()
count p =
  preConfCtx [ "label" .= LabelCount ] $ do
    countDecision (p ^. pStart)
    mapM_ countActivity (p ^. pTasks)

-- | Run counter from main with config file.
--
countMain :: MonadControl m => FilePath -> FilePath -> Maybe Text -> m ()
countMain cf pf domain =
  runCtx $ runTop $ do
    conf <- readYaml cf
    let conf' = override cDomain domain conf
    runConfCtx conf' $ do
      plans <- readYaml pf
      mapM_ count (plans :: [Plan])
