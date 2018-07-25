{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.Wolf.Ctx
  ( runTop
  , runConfCtx
  , preConfCtx
  , runAmazonCtx
  , runAmazonStoreCtx
  , runAmazonWorkCtx
  , runAmazonDecisionCtx
  ) where

import Control.Concurrent
import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Catch      (MonadMask)
import Control.Monad.Trans.AWS
import Data.Aeson
import Network.AWS.SWF
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types
import Network.HTTP.Types

-- | Catcher for exceptions, traces and rethrows.
--
botSomeExceptionCatch :: (MonadBase IO m, MonadCtx c m) => SomeException -> m a
botSomeExceptionCatch ex = do
  traceError "exception" [ "error" .= displayException ex ]
  throwIO ex

-- | Catch TransportError's.
--
botErrorCatch :: (MonadBase IO m, MonadCtx c m) => Error -> m a
botErrorCatch ex = do
  case ex of
    TransportError _ ->
      pure ()
    _ ->
      traceError "exception" [ "error" .= displayException ex ]
  throwIO ex

-- | Catcher for exceptions, emits stats and rethrows.
--
topSomeExceptionCatch :: (MonadBase IO m, MonadStatsCtx c m) => SomeException -> m a
topSomeExceptionCatch ex = do
  traceError "exception" [ "error" .= displayException ex ]
  statsIncrement "wolf.exception" [ "reason" =. textFromString (displayException ex) ]
  throwIO ex

-- | Run stats ctx.
--
runTop :: (MonadBaseControl IO m, MonadCtx c m) => TransT StatsCtx m a -> m a
runTop action = runStatsCtx $ catch action topSomeExceptionCatch

-- | Run bottom TransT.
--
runTrans :: (MonadIO m, MonadBaseControl IO m, MonadMask m, HasCtx c) => c -> TransT c m a -> m a
runTrans c action = runTransT c $ catches action [ Handler botErrorCatch, Handler botSomeExceptionCatch ]

-- | Run configuration context.
--
runConfCtx :: (MonadBaseControl IO m, MonadStatsCtx c m) => Conf -> TransT ConfCtx m a -> m a
runConfCtx conf action = do
  let preamble =
        [ "domain" .= (conf ^. cDomain)
        , "bucket" .= (conf ^. cBucket)
        , "prefix" .= (conf ^. cPrefix)
        ]
  c <- view statsCtx <&> cPreamble <>~ preamble
  runTrans (ConfCtx c conf) action

-- | Update configuration context's preamble.
--
preConfCtx :: (MonadBaseControl IO m, MonadConf c m) => Pairs -> TransT ConfCtx m a -> m a
preConfCtx preamble action = do
  c <- view confCtx <&> cPreamble <>~ preamble
  runTrans c action

-- | Run amazon context.
--
runAmazonCtx :: (MonadBaseControl IO m, MonadCtx c m) => TransT AmazonCtx m a -> m a
runAmazonCtx action = do
  c <- view ctx
#if MIN_VERSION_amazonka(1,4,5)
  e <- newEnv Discover
#else
  e <- newEnv Oregon Discover
#endif
  runTrans (AmazonCtx c e) action

-- | Run amazon store context.
--
runAmazonStoreCtx :: (MonadBaseControl IO m, MonadConf c m) => Text -> TransT AmazonStoreCtx m a -> m a
runAmazonStoreCtx uid action = do
  let preamble = [ "uid" .= uid ]
  c <- view confCtx <&> cPreamble <>~ preamble
  p <- (-/- uid) . view cPrefix <$> view ccConf
  runTrans (AmazonStoreCtx c p) action

-- | Throttle throttle exceptions.
--
throttled :: (MonadBaseControl IO m, MonadStatsCtx c m) => m a -> m a
throttled action = do
  traceError "throttled" mempty
  statsIncrement "wolf.throttled" mempty
  liftIO $ threadDelay $ 5 * 1000000
  catch action $ throttler action

-- | Amazon throttle handler.
--
throttler :: (MonadBase IO m, MonadBaseControl IO m, MonadStatsCtx c m) => m a -> Error -> m a
throttler action e =
  case e of
    ServiceError se ->
      bool (throwIO e) (throttled action) $
        se ^. serviceStatus == badRequest400 &&
        se ^. serviceCode == "Throttling"
    _ ->
      throwIO e

-- | Run amazon work context.
--
runAmazonWorkCtx :: (MonadBaseControl IO m, MonadConf c m) => Text -> TransT AmazonWorkCtx m a -> m a
runAmazonWorkCtx queue action = do
  let preamble = [ "queue" .= queue ]
  c <- view confCtx <&> cPreamble <>~ preamble
  runTrans (AmazonWorkCtx c queue) (catch action $ throttler action)

-- | Run amazon decision context.
--
runAmazonDecisionCtx :: (MonadBaseControl IO m, MonadConf c m) => Plan -> [HistoryEvent] -> TransT AmazonDecisionCtx m a -> m a
runAmazonDecisionCtx p hes action = do
  let preamble = [ "name" .= (p ^. pStart . tName) ]
  c <- view confCtx <&> cPreamble <>~ preamble
  runTrans (AmazonDecisionCtx c p hes) action
