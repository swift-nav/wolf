{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Local Prelude.
--
module Network.AWS.Wolf.Prelude
  ( module Exports
  , runResourceT
  , runConcurrent
  , stripPrefix'
  , MonadBaseControlIO
  , MonadMain
  ) where

import Control.Concurrent.Async.Lifted
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Text                       hiding (map)
import Preamble                        as Exports hiding (stripPrefix)

-- | Run a list of actions concurrently.
--
runConcurrent :: MonadBaseControl IO m => [m a] -> m ()
runConcurrent = void . runConcurrently . sequenceA . map Concurrently

-- | Strip the prefix with a '/' tacked on to the prefix.
--
stripPrefix' :: Text -> Text -> Maybe Text
stripPrefix' prefix =
  stripPrefix (prefix -/- mempty)

type MonadBaseControlIO m =
  ( MonadBaseControl IO m
  , MonadIO m
  )

type MonadMain m =
  ( MonadBaseControlIO m
  , MonadResource m
  , MonadCatch m
  )
