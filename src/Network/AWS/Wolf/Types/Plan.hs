{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Specifications for plans.
--
module Network.AWS.Wolf.Types.Plan where

import Data.Aeson.TH
import Network.AWS.Wolf.Aeson
import Network.AWS.Wolf.Prelude

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
