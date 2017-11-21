{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.Wolf.Ctx
  ( runConfCtx
  , preConfCtx
  , runAmazonCtx
  , runAmazonStoreCtx
  , runAmazonWorkCtx
  , runAmazonDecisionCtx
  ) where

import Control.Concurrent
import Control.Exception.Lifted
import Control.Monad.Trans.AWS
import Data.Aeson
import Network.AWS.SWF
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types
import Network.HTTP.Types

-- | Catcher for exceptions, traces and rethrows.
--
botSomeExceptionCatch :: MonadCtx c m => SomeException -> m a
botSomeExceptionCatch ex = do
  traceError "exception" [ "error" .= displayException ex ]
  throwIO ex

-- | Catch TransportError's.
--
botErrorCatch :: MonadCtx c m => Error -> m a
botErrorCatch ex = do
  case ex of
    TransportError _ ->
      pure ()
    _ ->
      traceError "exception" [ "error" .= displayException ex ]
  throwIO ex

-- | Catcher for exceptions, emits stats and rethrows.
--
topSomeExceptionCatch :: MonadStatsCtx c m => SomeException -> m a
topSomeExceptionCatch ex = do
  statsIncrement "wolf.exception" [ "reason" =. textFromString (displayException ex) ]
  throwIO ex

-- | Run bottom TransT.
--
runBotTransT :: (MonadControl m, HasCtx c) => c -> TransT c m a -> m a
runBotTransT c action = runTransT c $ catches action [ Handler botErrorCatch, Handler botSomeExceptionCatch ]

-- | Run top TransT.
--
runTopTransT :: (MonadControl m, HasStatsCtx c) => c -> TransT c m a -> m a
runTopTransT c action = runBotTransT c $ catch action topSomeExceptionCatch

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
  runTopTransT (ConfCtx c conf) action

-- | Update configuration context's preamble.
--
preConfCtx :: MonadConf c m => Pairs -> TransT ConfCtx m a -> m a
preConfCtx preamble action = do
  c <- view confCtx <&> cPreamble <>~ preamble
  runBotTransT c action

-- | Run amazon context.
--
runAmazonCtx :: MonadCtx c m => TransT AmazonCtx m a -> m a
runAmazonCtx action = do
  c <- view ctx
#if MIN_VERSION_amazonka(1,4,5)
  e <- newEnv Discover
#else
  e <- newEnv Oregon Discover
#endif
  runBotTransT (AmazonCtx c e) action

-- | Run amazon store context.
--
runAmazonStoreCtx :: MonadConf c m => Text -> TransT AmazonStoreCtx m a -> m a
runAmazonStoreCtx uid action = do
  let preamble = [ "uid" .= uid ]
  c <- view confCtx <&> cPreamble <>~ preamble
  p <- (-/- uid) . view cPrefix <$> view ccConf
  runBotTransT (AmazonStoreCtx c p) action

-- | Throttle throttle exceptions.
--
throttled :: MonadStatsCtx c m => m a -> m a
throttled action = do
  traceError "throttled" mempty
  statsIncrement "wolf.throttled" mempty
  liftIO $ threadDelay $ 5 * 1000000
  catch action $ throttler action

-- | Amazon throttle handler.
--
throttler :: MonadStatsCtx c m => m a -> Error -> m a
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
runAmazonWorkCtx :: MonadConf c m => Text -> TransT AmazonWorkCtx m a -> m a
runAmazonWorkCtx queue action = do
  let preamble = [ "queue" .= queue ]
  c <- view confCtx <&> cPreamble <>~ preamble
  runBotTransT (AmazonWorkCtx c queue) (catch action $ throttler action)

-- | Run amazon decision context.
--
runAmazonDecisionCtx :: MonadConf c m => Plan -> [HistoryEvent] -> TransT AmazonDecisionCtx m a -> m a
runAmazonDecisionCtx p hes action = do
  let preamble = [ "name" .= (p ^. pStart ^. tName) ]
  c <- view confCtx <&> cPreamble <>~ preamble
  runBotTransT (AmazonDecisionCtx c p hes) action
