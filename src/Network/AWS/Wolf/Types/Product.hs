{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Various product types.
--
module Network.AWS.Wolf.Types.Product where

import Data.Aeson.TH
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

-- | Plan Task
--
-- Work task.
--
data PlanTask = PlanTask
  { _ptName    :: Text
    -- ^ Name of task.
  , _ptVersion :: Text
    -- ^ Version of task.
  , _ptQueue   :: Text
    -- ^ Queue for task.
  , _ptTimeout :: Text
    -- ^ Timeout for task.
  } deriving (Show, Eq)

$(makeLenses ''PlanTask)
$(deriveJSON spinalOptions ''PlanTask)

-- | Plan
--
-- Group of tasks.
--
data Plan = Plan
  { _pStart :: PlanTask
    -- ^ Flow task.
  , _pTasks :: [PlanTask]
    -- ^ Worker tasks.
  } deriving (Show, Eq)

$(makeLenses ''Plan)
$(deriveJSON spinalOptions ''Plan)
