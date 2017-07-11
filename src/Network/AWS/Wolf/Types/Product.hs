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
  { _cDomain :: Text
    -- ^ SWF domain.
  , _cBucket :: Text
    -- ^ S3 bucket.
  , _cPrefix :: Text
    -- ^ S3 prefix.
  } deriving (Show, Eq)

$(makeLenses ''Conf)
$(deriveJSON spinalOptions ''Conf)

-- | Control
--
newtype Control = Control
  { _cRunUid :: Text
    -- ^ Run uid of workflow.
  } deriving (Show, Eq)

$(makeLenses ''Control)
$(deriveJSON spinalOptions ''Control)

-- | Plan Task
--
-- Work task.
--
data Task = Task
  { _tName    :: Text
    -- ^ Name of task.
  , _tVersion :: Text
    -- ^ Version of task.
  , _tQueue   :: Text
    -- ^ Queue for task.
  } deriving (Show, Eq)

$(makeLenses ''Task)
$(deriveJSON spinalOptions ''Task)

-- | Plan
--
-- Group of tasks.
--
data Plan = Plan
  { _pStart :: Task
    -- ^ Flow task.
  , _pTasks :: [Task]
    -- ^ Worker tasks.
  } deriving (Show, Eq)

$(makeLenses ''Plan)
$(deriveJSON spinalOptions ''Plan)
