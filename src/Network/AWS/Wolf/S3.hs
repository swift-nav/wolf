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
import Data.Conduit.Binary
import Data.Conduit.Combinators hiding (sinkFile, sourceFile)
import Data.Conduit.List        hiding (map, mapMaybe)
import Data.Text
import Network.AWS.Data.Body
import Network.AWS.Data.Text
import Network.AWS.S3           hiding (cBucket)
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types

-- | List keys in bucket with prefix.
--
listArtifacts :: MonadAmazonStore c m => m [Text]
listArtifacts = do
  b    <- view cBucket <$> view ccConf
  p    <- view ascPrefix
  lors <- paginate (set loPrefix (return p) $ listObjects (BucketName b)) $$ consume
  return $ mapMaybe (stripPrefix' p) $ toText . view oKey <$> join (view lorsContents <$> lors)

-- | Get object in bucket with key.
--
getArtifact :: MonadAmazonStore c m => FilePath -> Text -> m ()
getArtifact file key = do
  b    <- view cBucket <$> view ccConf
  p    <- view ascPrefix
  gors <- send $ getObject (BucketName b) (ObjectKey (p <\> key))
  sinkBody (gors ^. gorsBody) (sinkFile file)

-- | Put object in bucket with key.
--
putArtifact :: MonadAmazonStore c m => FilePath -> Text -> m ()
putArtifact file key = do
  b          <- view cBucket <$> view ccConf
  p          <- view ascPrefix
  (sha, len) <- sourceFile file $$ getZipSink $ (,) <$> ZipSink sinkSHA256 <*> ZipSink lengthE
  void $ send $ putObject (BucketName b) (ObjectKey (p <\> key)) $
    Hashed $ HashedStream sha len $ sourceFile file
