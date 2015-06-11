{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}

module Network.AWS.Flow.S3
  ( putObjectAction
  ) where

import Control.Monad.Reader      ( asks )
import Control.Monad.Trans.AWS   ( send_, sourceFileIO )
import Network.AWS.Flow.Types
import Network.AWS.Flow.Internal ( runAWS )
import Network.AWS.S3     hiding ( bucket )

-- Actions

putObjectAction :: MonadFlow m => Key -> FilePath -> m ()
putObjectAction key path = do
  body <- sourceFileIO path
  bucket <- asks feBucket
  runAWS feEnv $
    send_ $ putObject body bucket key
