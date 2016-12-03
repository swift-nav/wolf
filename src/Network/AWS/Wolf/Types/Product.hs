{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Various product types.
--
module Network.AWS.Wolf.Types.Product where

import Data.Aeson.TH
import Network.AWS.Wolf.Aeson
import Network.AWS.Wolf.Prelude

-- | Conf
--
-- SWF and S3 configuration parameters.
--
data Conf = Conf
  { _cTimeout     :: Int
    -- ^ SWF regular timeout.
  , _cPollTimeout :: Int
    -- ^ SWF polling timeout.
  , _cDomain      :: Text
    -- ^ SWF domain.
  , _cBucket      :: Text
    -- ^ S3 bucket.
  , _cPrefix      :: Text
    -- ^ S3 prefix.
  } deriving (Show, Eq)

$(makeLenses ''Conf)
$(deriveJSON spinalOptions ''Conf)

-- | Control
--
data Control = Control
  { _cRunUid :: Text
    -- ^ Run uid of workflow.
  } deriving (Show, Eq)

$(makeLenses ''Control)
$(deriveJSON spinalOptions ''Control)
