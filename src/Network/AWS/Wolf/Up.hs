{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Upload.
--
module Network.AWS.Wolf.Up
  ( up
  , upMain
  ) where

import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types

up :: MonadConf c m => FilePath -> Text -> m ()
up = undefined

upMain :: MonadControl m => FilePath -> FilePath -> Text -> m ()
upMain = undefined
