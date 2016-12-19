{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Local Prelude.
--
module Network.AWS.Wolf.Prelude
  ( module Exports
  , runResourceT
  , maybe_
  , eitherThrowIO
  , runConcurrent
  , textFromString
  , stripPrefix'
  , (<\>)
  , MonadBaseControlIO
  , MonadMain
  ) where

import BasicPrelude                    as Exports hiding (stripPrefix)
import Control.Concurrent.Async.Lifted
import Control.Lens                    as Exports hiding (uncons, (.=), (<.>))
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Text                       hiding (map)

-- | Maybe that returns () if Nothing
--
maybe_ :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybe_ = flip $ maybe $ return ()

-- | Throw userError on either error.
--
eitherThrowIO :: MonadIO m => Either String a -> m a
eitherThrowIO = either (liftIO . throwIO . userError) return

-- | Run a list of actions concurrently.
--
runConcurrent :: MonadBaseControl IO m => [m a] -> m ()
runConcurrent = void . runConcurrently . sequenceA . map Concurrently

-- | Reverse of textToString
--
textFromString :: String -> Text
textFromString = pack

-- | Strip the prefix with a '/' tacked on to the prefix.
--
stripPrefix' :: Text -> Text -> Maybe Text
stripPrefix' prefix =
  stripPrefix (prefix <\> mempty)

-- | </> for Text.
--
(<\>) :: Text -> Text -> Text
(<\>) = (<>) . (<> "/")

type MonadBaseControlIO m =
  ( MonadBaseControl IO m
  , MonadIO m
  )

type MonadMain m =
  ( MonadBaseControlIO m
  , MonadResource m
  , MonadCatch m
  )
