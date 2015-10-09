{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.AWS.Flow.Internal
  ( runFlowT
  , runDecideT
  , runAWS
  ) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Trans.AWS
import Data.Aeson
import Network.AWS.Flow.Types

-- FlowT

runFlowT :: FlowEnv -> FlowT m a -> m a
runFlowT e (FlowT m) = runReaderT m e

instance MonadThrow m => MonadThrow (FlowT m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (FlowT m) where
    catch (FlowT m) f = FlowT (catch m (unFlowT . f))

instance MonadBase b m => MonadBase b (FlowT m) where
    liftBase = liftBaseDefault

instance MonadTransControl FlowT where
    type StT FlowT a = StT (ReaderT FlowEnv) a

    liftWith = defaultLiftWith FlowT unFlowT
    restoreT = defaultRestoreT FlowT

instance MonadBaseControl b m => MonadBaseControl b (FlowT m) where
    type StM (FlowT m) a = ComposeSt FlowT m a

    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

instance MonadResource m => MonadResource (FlowT m) where
    liftResourceT = lift . liftResourceT

instance MonadError e m => MonadError e (FlowT m) where
    throwError     = lift . throwError
    catchError m f = FlowT (catchError (unFlowT m) (unFlowT . f))

instance Monad m => MonadReader FlowEnv (FlowT m) where
    ask     = FlowT ask
    local f = FlowT . local f . unFlowT
    reader  = FlowT . reader

-- DecideT

runDecideT :: DecideEnv -> DecideT m a -> m a
runDecideT e (DecideT m) = runReaderT m e

instance MonadThrow m => MonadThrow (DecideT m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (DecideT m) where
    catch (DecideT m) f = DecideT (catch m (unDecideT . f))

instance MonadBase b m => MonadBase b (DecideT m) where
    liftBase = liftBaseDefault

instance MonadTransControl DecideT where
    type StT DecideT a = StT (ReaderT DecideEnv) a

    liftWith = defaultLiftWith DecideT unDecideT
    restoreT = defaultRestoreT DecideT

instance MonadBaseControl b m => MonadBaseControl b (DecideT m) where
    type StM (DecideT m) a = ComposeSt DecideT m a

    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

instance MonadResource m => MonadResource (DecideT m) where
    liftResourceT = lift . liftResourceT

instance MonadError e m => MonadError e (DecideT m) where
    throwError     = lift . throwError
    catchError m f = DecideT (catchError (unDecideT m) (unDecideT . f))

instance Monad m => MonadReader DecideEnv (DecideT m) where
    ask     = DecideT ask
    local f = DecideT . local f . unDecideT
    reader  = DecideT . reader

-- Planning

instance FromJSON Plan where
  parseJSON (Object v) =
    Plan           <$>
      v .: "start" <*>
      v .: "specs" <*>
      v .: "end"
  parseJSON _ = mzero

instance FromJSON Spec where
  parseJSON (Object v) =
    msum
      [ Work           <$>
          v .: "work"
      , Sleep          <$>
          v .: "sleep"
      ]
  parseJSON _ =
    mzero

instance FromJSON End where
  parseJSON (String v)
    | v == "stop"     = return Stop
    | v == "continue" = return Continue
    | otherwise = mzero
  parseJSON _ = mzero

instance FromJSON Start where
  parseJSON (Object v) =
    Start         <$>
      v .: "flow"
  parseJSON _ = mzero

instance FromJSON Task where
  parseJSON (Object v) =
    Task             <$>
      v .: "name"    <*>
      v .: "version" <*>
      v .: "queue"   <*>
      v .: "timeout"
  parseJSON _ = mzero

instance FromJSON Timer where
  parseJSON (Object v) =
    Timer            <$>
      v .: "name"    <*>
      v .: "timeout"
  parseJSON _ = mzero

-- Helpers

runAWS :: MonadFlow m => (FlowEnv -> Seconds) -> AWST m a -> m a
runAWS to action = do
  e <- ask
  runAWST (feEnv e) $ timeout (to e) $ action


