{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Context objects for monad transformers.
--
module Network.AWS.Wolf.Types.Ctx where

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Network.AWS.Wolf.Lens
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types.Alias
import Network.AWS.Wolf.Types.Product

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
  ( MonadBaseControlIO m
  , MonadReader c m
  , MonadLogger m
  , MonadCatch m
  , MonadResource m
  , HasCtx c
  )

-- | ConfCtx
--
-- Configuration context.
--
data ConfCtx = ConfCtx
  { _ccCtx  :: Ctx
    -- ^ Parent context.
  , _ccConf :: Conf
    -- ^ Configuration parameters.
  }

$(makeClassyConstraints ''ConfCtx [''HasCtx])

instance HasCtx ConfCtx where
  ctx = ccCtx

type MonadConf c m =
  ( MonadCtx c m
  , HasConfCtx c
  )

-- | AmazonCtx
--
-- Amazon context.
--
data AmazonCtx = AmazonCtx
  { _acConfCtx :: ConfCtx
    -- ^ Parent context.
  , _acEnv     :: Env
    -- ^ Amazon environment.
  }

$(makeClassyConstraints ''AmazonCtx [''HasConfCtx, ''HasEnv])

instance HasConfCtx AmazonCtx where
  confCtx = acConfCtx

instance HasCtx AmazonCtx where
  ctx = confCtx . ccCtx

instance HasEnv AmazonCtx where
  environment = acEnv

type MonadAmazon c m =
  ( MonadConf c m
  , HasAmazonCtx c
  , AWSConstraint c m
  )

-- | AmazonStoreCtx
--
-- Amazon store context.
--
data AmazonStoreCtx = AmazonStoreCtx
  { _ascAmazonCtx :: AmazonCtx
    -- ^ Parent context.
  , _ascUid       :: Text
    -- ^ Workflow uid.
  , _ascPrefix    :: Text
    -- ^ Object prefix.
  }

$(makeClassyConstraints ''AmazonStoreCtx [''HasAmazonCtx])

instance HasAmazonCtx AmazonStoreCtx where
  amazonCtx = ascAmazonCtx

instance HasConfCtx AmazonStoreCtx where
   confCtx = amazonCtx . acConfCtx

instance HasCtx AmazonStoreCtx where
   ctx = confCtx . ccCtx

instance HasEnv AmazonStoreCtx where
   environment = amazonCtx . acEnv

type MonadAmazonStore c m =
   ( MonadAmazon c m
   , HasAmazonStoreCtx c
   )

-- | AmazonStoreCtx
--
-- Amazon work context.
--
data AmazonWorkCtx = AmazonWorkCtx
  { _awcAmazonCtx :: AmazonCtx
    -- ^ Parent context.
  , _awcQueue     :: Text
    -- ^ Workflow queue.
  }

$(makeClassyConstraints ''AmazonWorkCtx [''HasAmazonCtx])

instance HasAmazonCtx AmazonWorkCtx where
  amazonCtx = awcAmazonCtx

instance HasConfCtx AmazonWorkCtx where
   confCtx = amazonCtx . acConfCtx

instance HasCtx AmazonWorkCtx where
   ctx = confCtx . ccCtx

instance HasEnv AmazonWorkCtx where
   environment = amazonCtx . acEnv

type MonadAmazonWork c m =
   ( MonadAmazon c m
   , HasAmazonWorkCtx c
   )
