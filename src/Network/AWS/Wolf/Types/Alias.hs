{-# LANGUAGE NoImplicitPrelude #-}

-- | Various alias types.
--
module Network.AWS.Wolf.Types.Alias where

import Control.Monad.Logger
import Data.Aeson
import Network.AWS.Wolf.Prelude

-- | Pairs
--
type Pairs = [(Text, Value)]

-- | Trace
--
type Trace = Loc -> LogSource -> LogLevel -> LogStr -> IO ()
