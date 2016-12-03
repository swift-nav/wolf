module Network.AWS.Flow.S3
  ( listObjectsAction
  , getObjectAction
  , putObjectAction
  ) where

import Network.AWS.Flow.Prelude hiding (ByteString, hash, stripPrefix)
import Network.AWS.Flow.Types

import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List        hiding (concatMap, map, mapMaybe)
import Data.Text                hiding (concatMap, map)
import Network.AWS.Data.Body
import Network.AWS.Data.Text
import Network.AWS.S3

-- Actions

listObjectsAction :: MonadFlow m => Uid -> m [Text]
listObjectsAction uid = do
  timeout' <- asks feTimeout
  bucket' <- asks feBucket
  prefix <- asks fePrefix
  timeout timeout' $ do
    rs <- paginate (listObjects (BucketName bucket') &
      loPrefix .~ Just (prefix <> "/" <> uid))
        $$ consume
    return $
      mapMaybe (stripPrefix (prefix <> "/" <> uid <> "/") . toText . (^. oKey)) $
        concatMap (^. lorsContents) rs

getObjectAction :: MonadFlow m => Uid -> Text -> m Blob
getObjectAction uid key = do
  timeout' <- asks feTimeout
  bucket' <- asks feBucket
  prefix <- asks fePrefix
  timeout timeout' $ do
    r <- send $ getObject (BucketName bucket') (ObjectKey $ prefix <> "/" <> uid <> "/" <> key)
    blob <- sinkBody (r ^. gorsBody) sinkLbs
    return (key, blob)

putObjectAction :: MonadFlow m => Uid -> Artifact -> m ()
putObjectAction uid (key, hash, size, blob) = do
  timeout' <- asks feTimeout
  bucket' <- asks feBucket
  prefix <- asks fePrefix
  void $ timeout timeout' $
    send $ putObject (BucketName bucket') (ObjectKey $ prefix <> "/" <> uid <> "/" <> key) (Hashed $ hashedBody hash size $ sourceLbs blob)
