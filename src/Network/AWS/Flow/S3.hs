{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}

module Network.AWS.Flow.S3
  ( putObjectAction
  ) where

import Control.Monad.Reader      ( asks )
import Control.Monad.Trans.AWS   ( send_, sourceBody )
import Data.Conduit.Binary       ( sourceLbs )
import Network.AWS.Flow.Types
import Network.AWS.Flow.Internal ( runAWS )
import Network.AWS.S3     hiding ( bucket )

-- Actions

putObjectAction :: MonadFlow m => Artifact -> m ()
putObjectAction (key, hash, size, blob) = do
  bucket <- asks feBucket
  runAWS feEnv $
    send_ $ putObject body bucket key where
      body = sourceBody hash size $ sourceLbs blob
