{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | S3 Calls.
--
module Network.AWS.Wolf.S3
  ( listArtifacts
  , getArtifact
  , putArtifact
  ) where

import Control.Monad.Trans.AWS
import Data.Conduit
import Data.Conduit.Combinators
import Network.AWS.Data
import Network.AWS.Data.Body
import Network.AWS.S3           hiding (cBucket)
import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.Prelude hiding (concatMap)
import Network.AWS.Wolf.Types

-- | List keys in bucket with prefix.
--
listArtifacts :: MonadAmazonStore c m => m [Text]
listArtifacts = do
  b <- view cBucket <$> view ccConf
  p <- view ascPrefix
  runResourceT $ runAmazonCtx $
    paginate (set loPrefix (pure p) $ listObjects (BucketName b))
      =$= concatMap (((toText . view oKey) <$>) . view lorsContents)
      $$  sinkList

-- | Get object in bucket with key.
--
getArtifact :: MonadAmazonStore c m => FilePath -> Text -> m ()
getArtifact file key = do
  b <- view cBucket <$> view ccConf
  p <- view ascPrefix
  runResourceT $ runAmazonCtx $ do
    gors <- send $ getObject (BucketName b) (ObjectKey (p -/- key))
    sinkBody (gors ^. gorsBody) (sinkFileBS file)

-- | Put object in bucket with key.
--
putArtifact :: MonadAmazonStore c m => FilePath -> Text -> m ()
putArtifact file key = do
  b <- view cBucket <$> view ccConf
  p <- view ascPrefix
  runResourceT $ runAmazonCtx $ do
    (sha, len) <- sourceFileBS file $$ getZipSink $ (,) <$> ZipSink sinkSHA256 <*> ZipSink lengthE
    void $ send $ putObject (BucketName b) (ObjectKey (p -/- key)) $
      Hashed $ HashedStream sha len $ sourceFileBS file

