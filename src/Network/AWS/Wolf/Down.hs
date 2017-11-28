{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Download.
--
module Network.AWS.Wolf.Down
  ( down
  , downMain
  ) where

import Control.Concurrent.Async.Lifted
import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.File
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types
import Network.AWS.Wolf.S3

downer :: MonadAmazonStore c m => FilePath -> FilePath -> m ()
downer dir key = do
  traceInfo "download" [ "dir" .= dir, "key" .= key ]
  touchDirectory (dir </> key)
  getArtifact (dir </> key) key

-- | Download workflow to directory.
--
down :: MonadConf c m => FilePath -> Text -> m ()
down dir uid =
  preConfCtx [ "label" .= LabelAct ] $
    runAmazonStoreCtx uid $ do
      traceInfo "download" [ "dir" .= dir ]
      ks <- listArtifacts
      void $ runConcurrently $ sequenceA $ map (\x -> Concurrently $ downer dir x) ks

downer' :: MonadAmazon c m => FilePath -> Text -> Text -> FilePath -> m ()
downer' dir b p key = do
  touchDirectory (dir </> key)
  getArtifact' (dir </> key) b (p -/- textFromString key)

down' :: MonadConf c m => FilePath -> Text -> m ()
down' dir uid =
  preConfCtx [ "label" .= LabelAct ] $
    runAmazonStoreCtx uid $ do
      b <- view cBucket <$> view ccConf
      p <- view ascPrefix
      runResourceT $ runAmazonCtx $ do
        ks <- listArtifacts' b p
        void $ runConcurrently $ sequenceA $ map (\x -> Concurrently $ downer' dir b p x) ks

-- | Run download from main with config file.
--
downMain :: MonadControl m => FilePath -> FilePath -> Text -> m ()
downMain cf dir uid =
  runCtx $
    runStatsCtx $ do
      conf <- readYaml cf
      runConfCtx conf $
         down' dir uid
