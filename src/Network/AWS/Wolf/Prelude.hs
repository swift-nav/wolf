{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Local Prelude.
--
module Network.AWS.Wolf.Prelude
  ( module Exports
  , runConcurrent
  , override
  , interleave
  ) where

import Control.Concurrent.Async.Lifted
import Preamble                        as Exports

-- | Run a list of actions concurrently.
--
runConcurrent :: MonadBaseControl IO m => [m a] -> m ()
runConcurrent = void . runConcurrently . traverse Concurrently

-- | Override a lens value with a maybe value.
--
override :: ASetter s s a b -> Maybe b -> s -> s
override k v c = maybe c (flip (set k) c) v

-- | Interleave arrays.
--
interleave :: [a] -> [a] -> [a]
interleave xs ys = concat $ zipWith (\x y -> [x, y]) xs ys
