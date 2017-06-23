{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Context objects for monad transformers.
--
module Network.AWS.Wolf.Types.Ctx where

import Control.Monad.Trans.AWS
import Network.AWS.SWF
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types.Product

-- | ConfCtx
--
-- Configuration context.
--
data ConfCtx = ConfCtx
  { _ccStatsCtx :: StatsCtx
    -- ^ Parent context.
  , _ccConf     :: Conf
    -- ^ Configuration parameters.
  }

$(makeClassyConstraints ''ConfCtx [''HasStatsCtx])

instance HasStatsCtx ConfCtx where
  statsCtx = ccStatsCtx

instance HasCtx ConfCtx where
  ctx = statsCtx . ctx

type MonadConf c m =
  ( MonadStatsCtx c m
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

instance HasStatsCtx AmazonCtx where
  statsCtx = confCtx . statsCtx

instance HasCtx AmazonCtx where
  ctx = statsCtx . ctx

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
  { _ascConfCtx :: ConfCtx
    -- ^ Parent context.
  , _ascUid     :: Text
    -- ^ Workflow uid.
  , _ascPrefix  :: Text
    -- ^ Object prefix.
  }

$(makeClassyConstraints ''AmazonStoreCtx [''HasConfCtx])

instance HasConfCtx AmazonStoreCtx where
   confCtx = ascConfCtx

instance HasStatsCtx AmazonStoreCtx where
  statsCtx = confCtx . statsCtx

instance HasCtx AmazonStoreCtx where
  ctx = statsCtx . ctx

type MonadAmazonStore c m =
   ( MonadConf c m
   , HasAmazonStoreCtx c
   )

-- | AmazonWorkCtx
--
-- Amazon work context.
--
data AmazonWorkCtx = AmazonWorkCtx
  { _awcConfCtx :: ConfCtx
    -- ^ Parent context.
  , _awcQueue   :: Text
    -- ^ Workflow queue.
  }

$(makeClassyConstraints ''AmazonWorkCtx [''HasConfCtx])

instance HasConfCtx AmazonWorkCtx where
   confCtx = awcConfCtx

instance HasStatsCtx AmazonWorkCtx where
  statsCtx = confCtx . statsCtx

instance HasCtx AmazonWorkCtx where
  ctx = statsCtx . ctx

type MonadAmazonWork c m =
   ( MonadConf c m
   , HasAmazonWorkCtx c
   )

-- | AmazonDecisionCtx
--
-- Amazon decision context.
--
data AmazonDecisionCtx = AmazonDecisionCtx
  { _adcConfCtx :: ConfCtx
    -- ^ Parent context.
  , _adcPlan    :: Plan
    -- ^ Decision plan.
  , _adcEvents  :: [HistoryEvent]
    -- ^ History events.
  }

$(makeClassyConstraints ''AmazonDecisionCtx [''HasConfCtx])

instance HasConfCtx AmazonDecisionCtx where
   confCtx = adcConfCtx

instance HasStatsCtx AmazonDecisionCtx where
  statsCtx = confCtx . statsCtx

instance HasCtx AmazonDecisionCtx where
  ctx = statsCtx . ctx

type MonadAmazonDecision c m =
   ( MonadConf c m
   , HasAmazonDecisionCtx c
   )

