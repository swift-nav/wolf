{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.Wolf.Ctx
  ( runCtx
  , preCtx
  , runConfCtx
  , preConfCtx
  , runAmazonCtx
  , preAmazonCtx
  , runAmazonStoreCtx
  , preAmazonStoreCtx
  , runAmazonWorkCtx
  , preAmazonWorkCtx
  ) where

import Control.Monad.Trans.AWS
import Data.Aeson
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types

-- | Run configuration context.
--
runConfCtx :: MonadCtx c m => Conf -> TransT ConfCtx m a -> m a
runConfCtx conf action = do
  let preamble =
        [ "domain" .= (conf ^. cDomain)
        , "bucket" .= (conf ^. cBucket)
        , "prefix" .= (conf ^. cPrefix)
        ]
  c <- view ctx <&> cPreamble <>~ preamble
  runTransT (ConfCtx c conf) action

-- | Update configuration context's preamble.
--
preConfCtx :: MonadConf c m => Pairs -> TransT ConfCtx m a -> m a
preConfCtx preamble action = do
  c <- view confCtx <&> cPreamble <>~ preamble
  runTransT c action

-- | Run amazon context.
--
runAmazonCtx :: MonadConf c m => TransT AmazonCtx m a -> m a
runAmazonCtx action = do
  c <- view confCtx
  e <- newEnv Oregon $ FromEnv "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY" mempty
  runTransT (AmazonCtx c e) action

-- | Update amazon context's preamble.
--
preAmazonCtx :: MonadAmazon c m => Pairs -> TransT AmazonCtx m a -> m a
preAmazonCtx preamble action = do
  c <- view amazonCtx <&> cPreamble <>~ preamble
  runTransT c action

-- | Run amazon store context.
--
runAmazonStoreCtx :: MonadAmazon c m => Text -> TransT AmazonStoreCtx m a -> m a
runAmazonStoreCtx uid action = do
  let preamble = [ "uid" .= uid ]
  c <- view amazonCtx <&> cPreamble <>~ preamble
  p <- (-/- uid) . view cPrefix <$> view ccConf
  runTransT (AmazonStoreCtx c uid p) action

-- | Update amazon context's preamble.
--
preAmazonStoreCtx :: MonadAmazonStore c m => Pairs -> TransT AmazonStoreCtx m a -> m a
preAmazonStoreCtx preamble action = do
  c <- view amazonStoreCtx <&> cPreamble <>~ preamble
  runTransT c action

-- | Run amazon work context.
--
runAmazonWorkCtx :: MonadAmazon c m => Text -> TransT AmazonWorkCtx m a -> m a
runAmazonWorkCtx queue action = do
  let preamble = [ "queue" .= queue ]
  c <- view amazonCtx <&> cPreamble <>~ preamble
  runTransT (AmazonWorkCtx c queue) action

-- | Update amazon context's preamble.
--
preAmazonWorkCtx :: MonadAmazonWork c m => Pairs -> TransT AmazonWorkCtx m a -> m a
preAmazonWorkCtx preamble action = do
  c <- view amazonWorkCtx <&> cPreamble <>~ preamble
  runTransT c action
