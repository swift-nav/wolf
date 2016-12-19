{-# LANGUAGE NoImplicitPrelude #-}

-- | SWF Decider logic.
--
module Network.AWS.Wolf.Decide
  ( decide
  , decideMain
  ) where

import Data.Aeson
import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.File
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.SWF
import Network.AWS.Wolf.Types

-- | Decider logic - poll for decisions, make decisions.
--
decide :: MonadConf c m => Plan -> m ()
decide plan =
  preConfCtx [ "label" .= LabelDecide ] $
    runAmazonCtx $
      runAmazonWorkCtx (plan ^. pStart ^. ptQueue) $ do
        traceInfo "poll" mempty
        (token, _events) <- pollDecision
        maybe_ token $ \_token' -> do
          traceInfo "start" mempty
          traceInfo "finish" mempty

-- | Run decider from main with config file.
--
decideMain :: MonadMain m => FilePath -> FilePath -> m ()
decideMain cf pf =
  runCtx $ do
    conf <- readYaml cf
    runConfCtx conf $ do
      plans <- readYaml pf
      runConcurrent $
        (forever . decide) <$> plans
