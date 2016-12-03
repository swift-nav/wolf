{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Context objects for monad transformers.
--
module Network.AWS.Wolf.Types.Ctx where

import Control.Monad.Logger
import Control.Monad.Reader
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types.Alias

-- | Ctx
--
-- Base context, supports tracing.
--
data Ctx = Ctx
  { _cPreamble :: Pairs
    -- ^ Object to encode on every trace line.
  , _cTrace    :: Trace
    -- ^ Configurable tracing function.
  }

$(makeClassy ''Ctx)

type MonadCtx c m =
  ( MonadIO m
  , MonadReader c m
  , MonadLogger m
  , HasCtx c
  )
