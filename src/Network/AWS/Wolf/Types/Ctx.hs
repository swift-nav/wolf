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

instance HasStatsCtx AmazonStoreCtx where
  statsCtx = confCtx . statsCtx

instance HasCtx AmazonStoreCtx where
  ctx = statsCtx . ctx

instance HasEnv AmazonStoreCtx where
   environment = amazonCtx . acEnv

type MonadAmazonStore c m =
   ( MonadAmazon c m
   , HasAmazonStoreCtx c
   )

-- | AmazonWorkCtx
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

instance HasStatsCtx AmazonWorkCtx where
  statsCtx = confCtx . statsCtx

instance HasCtx AmazonWorkCtx where
  ctx = statsCtx . ctx

instance HasEnv AmazonWorkCtx where
   environment = amazonCtx . acEnv

type MonadAmazonWork c m =
   ( MonadAmazon c m
   , HasAmazonWorkCtx c
   )

-- | AmazonDecisionCtx
--
-- Amazon decision context.
--
data AmazonDecisionCtx = AmazonDecisionCtx
  { _adcAmazonCtx :: AmazonCtx
    -- ^ Parent context.
  , _adcPlan      :: Plan
    -- ^ Decision plan.
  , _adcEvents    :: [HistoryEvent]
    -- ^ History events.
  }

$(makeClassyConstraints ''AmazonDecisionCtx [''HasAmazonCtx])

instance HasAmazonCtx AmazonDecisionCtx where
  amazonCtx = adcAmazonCtx

instance HasConfCtx AmazonDecisionCtx where
   confCtx = amazonCtx . acConfCtx

instance HasStatsCtx AmazonDecisionCtx where
  statsCtx = confCtx . statsCtx

instance HasCtx AmazonDecisionCtx where
  ctx = statsCtx . ctx

instance HasEnv AmazonDecisionCtx where
   environment = amazonCtx . acEnv

type MonadAmazonDecision c m =
   ( MonadAmazon c m
   , HasAmazonDecisionCtx c
   )

