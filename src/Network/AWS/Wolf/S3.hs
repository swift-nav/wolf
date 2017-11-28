{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | S3 Calls.
--
module Network.AWS.Wolf.S3
  ( listArtifacts
  , getArtifact
  , putArtifact
  , listArtifacts'
  , getArtifact'
  ) where

import Control.Monad.Trans.AWS
import Data.Conduit
import Data.Conduit.Combinators
import Network.AWS.Data
import Network.AWS.S3           hiding (cBucket)
import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.Prelude hiding (concatMap)
import Network.AWS.Wolf.Types
import System.FilePath

-- | List keys in bucket with prefix.
--
listArtifacts :: MonadAmazonStore c m => m [FilePath]
listArtifacts = do
  b <- view cBucket <$> view ccConf
  p <- view ascPrefix
  runResourceT $ runAmazonCtx $
    paginate (set loPrefix (pure p) $ listObjects (BucketName b))
      =$= concatMap ((makeRelative (textToString p) . textToString . toText . view oKey <$>) . view lorsContents)
      $$  sinkList

listArtifacts' :: MonadAmazon c m => Text -> Text -> m [FilePath]
listArtifacts' b p = do
  paginate (set loPrefix (pure p) $ listObjects (BucketName b))
    =$= concatMap ((makeRelative (textToString p) . textToString . toText . view oKey <$>) . view lorsContents)
    $$  sinkList

-- | Get object in bucket with key.
--
getArtifact :: MonadAmazonStore c m => FilePath -> FilePath -> m ()
getArtifact file key = do
  b <- view cBucket <$> view ccConf
  p <- view ascPrefix
  runResourceT $ runAmazonCtx $ do
    gors <- send $ getObject (BucketName b) (ObjectKey (p -/- textFromString key))
    sinkBody (gors ^. gorsBody) (sinkFileBS file)

getArtifact' :: MonadAmazon c m => FilePath -> Text -> Text -> m ()
getArtifact' f b k = do
  gors <- send $ getObject (BucketName b) (ObjectKey k)
  sinkBody (gors ^. gorsBody) (sinkFileBS f)
-- | Put object in bucket with key.
--
putArtifact :: MonadAmazonStore c m => FilePath -> FilePath -> m ()
putArtifact file key = do
  b <- view cBucket <$> view ccConf
  p <- view ascPrefix
  runResourceT $ runAmazonCtx $ do
    body <- chunkedFile defaultChunkSize file
    void $ send $ putObject (BucketName b) (ObjectKey (p -/- textFromString key)) body
