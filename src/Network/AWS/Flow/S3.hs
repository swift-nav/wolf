module Network.AWS.Flow.S3
  ( putObjectAction
  ) where

import Network.AWS.Data.Body
import Control.Monad.Reader
import Control.Monad.Trans.AWS
import Data.Conduit.Binary
import Data.Monoid
import Network.AWS.Flow.Types
import Network.AWS.Flow.Internal
import Network.AWS.S3

-- Actions

putObjectAction :: MonadFlow m => Artifact -> m ()
putObjectAction (key, hash, size, blob) = do
  e <- ask
  runAWS feTimeout $ void $
    send $ putObject
             (BucketName $ feBucket e)
             (ObjectKey $ (fePrefix e) <> "/" <> key)
             (Hashed $ hashedBody hash size $ sourceLbs blob)
