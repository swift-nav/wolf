{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}

module Network.AWS.Flow.S3
  ( putObjectAction
  ) where

import Control.Monad.Trans.AWS   ( send_, sourceFileIO )
import Network.AWS.Flow.Types
import Network.AWS.Flow.Internal ( runAWS )
import Network.AWS.S3

-- Actions

putObjectAction :: MonadFlow m => Pail -> Key -> FilePath -> m ()
putObjectAction pail key path = do
  body <- sourceFileIO path
  runAWS feEnv $
    send_ $ putObject body pail key
