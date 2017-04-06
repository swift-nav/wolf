{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.Wolf.Ctx
  ( runConfCtx
  , preConfCtx
  , runAmazonCtx
  , preAmazonCtx
  , runAmazonStoreCtx
  , preAmazonStoreCtx
  , runAmazonWorkCtx
  , preAmazonWorkCtx
  , runAmazonDecisionCtx
  , preAmazonDecisionCtx
  ) where

import Control.Concurrent
import Control.Monad.Trans.AWS
import Data.Aeson
import Network.AWS.SWF
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types
import Network.HTTP.Types

-- | Handler for exceptions, traces and rethrows.
--
catcher :: MonadStatsCtx c m => SomeException -> m a
catcher e = do
  traceError "exception" [ "error" .= displayException e ]
  throwIO e

-- | Run configuration context.
--
runConfCtx :: MonadStatsCtx c m => Conf -> TransT ConfCtx m a -> m a
runConfCtx conf action = do
  let preamble =
        [ "domain" .= (conf ^. cDomain)
        , "bucket" .= (conf ^. cBucket)
        , "prefix" .= (conf ^. cPrefix)
        ]
  c <- view statsCtx <&> cPreamble <>~ preamble
  runTransT (ConfCtx c conf) $ catch action catcher

-- | Update configuration context's preamble.
--
preConfCtx :: MonadConf c m => Pairs -> TransT ConfCtx m a -> m a
preConfCtx preamble action = do
  c <- view confCtx <&> cPreamble <>~ preamble
  runTransT c $ catch action catcher

-- | Run amazon context.
--
runAmazonCtx :: MonadConf c m => TransT AmazonCtx m a -> m a
runAmazonCtx action = do
  c <- view confCtx
  e <- newEnv Oregon $ FromEnv "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY" mempty
  runTransT (AmazonCtx c e) $ catch action catcher

-- | Update amazon context's preamble.
--
preAmazonCtx :: MonadAmazon c m => Pairs -> TransT AmazonCtx m a -> m a
preAmazonCtx preamble action = do
  c <- view amazonCtx <&> cPreamble <>~ preamble
  runTransT c $ catch action catcher

-- | Run amazon store context.
--
runAmazonStoreCtx :: MonadAmazon c m => Text -> TransT AmazonStoreCtx m a -> m a
runAmazonStoreCtx uid action = do
  let preamble = [ "uid" .= uid ]
  c <- view amazonCtx <&> cPreamble <>~ preamble
  p <- (-/- uid) . view cPrefix <$> view ccConf
  runTransT (AmazonStoreCtx c uid p) $ catch action catcher

-- | Update amazon context's preamble.
--
preAmazonStoreCtx :: MonadAmazonStore c m => Pairs -> TransT AmazonStoreCtx m a -> m a
preAmazonStoreCtx preamble action = do
  c <- view amazonStoreCtx <&> cPreamble <>~ preamble
  runTransT c $ catch action catcher

-- | Amazon throttle handler.
--
throttler :: MonadAmazon c m => m a -> Error -> m a
throttler action e =
  case e of
    ServiceError se -> do
      let delay = liftIO $ threadDelay $ 5 * 1000000
      bool (throwIO e) (delay >> catch action (throttler action)) $
        se ^. serviceStatus == badRequest400 &&
        se ^. serviceCode == "Throttling"
    _ ->
      throwIO e

-- | Run amazon work context.
--
runAmazonWorkCtx :: MonadAmazon c m => Text -> TransT AmazonWorkCtx m a -> m a
runAmazonWorkCtx queue action = do
  let preamble = [ "queue" .= queue ]
  c <- view amazonCtx <&> cPreamble <>~ preamble
  runTransT (AmazonWorkCtx c queue) $ catch (catch action $ throttler action) catcher

-- | Update amazon context's preamble.
--
preAmazonWorkCtx :: MonadAmazonWork c m => Pairs -> TransT AmazonWorkCtx m a -> m a
preAmazonWorkCtx preamble action = do
  c <- view amazonWorkCtx <&> cPreamble <>~ preamble
  runTransT c $ catch action catcher

-- | Run amazon decision context.
--
runAmazonDecisionCtx :: MonadAmazon c m => Plan -> [HistoryEvent] -> TransT AmazonDecisionCtx m a -> m a
runAmazonDecisionCtx p hes action = do
  let preamble = [ "name" .= (p ^. pStart ^. tName) ]
  c <- view amazonCtx <&> cPreamble <>~ preamble
  runTransT (AmazonDecisionCtx c p hes) $ catch action catcher

-- | Update amazon context's preamble.
--
preAmazonDecisionCtx :: MonadAmazonDecision c m => Pairs -> TransT AmazonDecisionCtx m a -> m a
preAmazonDecisionCtx preamble action = do
  c <- view amazonDecisionCtx <&> cPreamble <>~ preamble
  runTransT c $ catch action catcher
