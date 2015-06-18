{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}

module Network.AWS.Flow.S3
  ( putObjectAction
  ) where

import Control.Monad.Reader      ( asks )
import Control.Monad.Trans.AWS   ( send_, sourceBody )
import Data.Conduit.Binary       ( sourceLbs )
import Data.Text                 ( concat )
import Network.AWS.Flow.Types
import Network.AWS.Flow.Internal ( runAWS )
import Network.AWS.S3     hiding ( bucket )
import Prelude            hiding ( concat )

-- Actions

putObjectAction :: MonadFlow m => Artifact -> m ()
putObjectAction (key, hash, size, blob) = do
  bucket <- asks feBucket
  prefix <- asks fePrefix
  runAWS feEnv $
    send_ $ putObject body bucket $ concat [prefix, "/", key] where
      body = sourceBody hash size $ sourceLbs blob
