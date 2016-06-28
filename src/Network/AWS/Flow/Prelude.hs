module Network.AWS.Flow.Prelude
  ( module BasicPrelude
  , module Control.Lens
  , module Control.Monad.Reader
  , module Control.Monad.Trans.AWS
  , maybe_
  , maybe'
  ) where

import BasicPrelude hiding ( (<.>), uncons )
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.AWS

maybe_ :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybe_ m f = maybe (return ()) f m

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m b a = maybe b a m
