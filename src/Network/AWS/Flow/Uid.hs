module Network.AWS.Flow.Uid
  ( newUid
  ) where

import Network.AWS.Flow.Prelude
import Network.AWS.Flow.Types

import Data.UUID
import Data.UUID.V4

newUid :: MonadIO m => m Uid
newUid = toText <$> liftIO nextRandom
