{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Various sum types.
--
module Network.AWS.Wolf.Types.Sum where

import Data.Aeson.TH
import Network.AWS.Wolf.Prelude

-- | LabelType
--
-- Tags for referencing workers.
--
data LabelType
  = LabelAct
  | LabelDecide
  | LabelCount
  deriving (Show, Eq)

$(deriveJSON spinalOptions ''LabelType)
