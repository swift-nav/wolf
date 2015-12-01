module Network.AWS.Flow.S3
  ( listObjectsAction
  , getObjectAction
  , putObjectAction
  ) where

import Network.AWS.Flow.Prelude hiding ( ByteString, hash )
import Network.AWS.Flow.Types

import Data.ByteString.Lazy hiding ( concatMap, map )
import Data.Conduit
import Data.Conduit.List hiding ( concatMap, map )
import Data.Conduit.Binary
import Network.AWS.Data.Body
import Network.AWS.S3

-- Actions

listObjectsAction :: MonadFlow m => m [ObjectKey]
listObjectsAction = do
  timeout' <- asks feTimeout
  bucket' <- asks feBucket
  prefix <- asks fePrefix
  timeout timeout' $ do
    rs <- paginate (listObjects (BucketName bucket') &
      loPrefix .~ Just prefix)
        $$ consume
    return $
      map (^. oKey) $
        concatMap (^. lorsContents) rs

getObjectAction :: MonadFlow m => ObjectKey -> m ByteString
getObjectAction key = do
  timeout' <- asks feTimeout
  bucket' <- asks feBucket
  timeout timeout' $ do
    r <- send $ getObject (BucketName bucket') key
    sinkBody (r ^. gorsBody) sinkLbs

putObjectAction :: MonadFlow m => Artifact -> m ()
putObjectAction (key, hash, size, blob) = do
  timeout' <- asks feTimeout
  bucket' <- asks feBucket
  prefix <- asks fePrefix
  void $ timeout timeout' $
    send $ putObject (BucketName bucket') (ObjectKey $ prefix <> "/" <> key) (Hashed $ hashedBody hash size $ sourceLbs blob)
