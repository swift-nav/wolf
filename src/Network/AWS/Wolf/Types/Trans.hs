{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Monad transformer.
--
module Network.AWS.Wolf.Types.Trans where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Network.AWS.Wolf.Prelude

-- | Monad transformer for reading and logging.
--
newtype TransT c m a = TransT
  { unTransT :: LoggingT (ReaderT c m) a
    -- ^ LoggingT and ReaderT transformer.
  } deriving (Functor, Applicative, Monad, MonadLogger, MonadReader c, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadBase b m => MonadBase b (TransT c m) where
  liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (TransT c m) where
    type StM (TransT c m) a = ComposeSt (TransT c) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance MonadTransControl (TransT c) where
  type StT (TransT c) a = StT (ReaderT c) a
  liftWith f = TransT $
    liftWith $ \g ->
      liftWith $ \h ->
        f (h . g . unTransT)
  restoreT = TransT . restoreT . restoreT

instance MonadTrans (TransT c) where
  lift = TransT . lift . lift

instance MonadResource m => MonadResource (TransT c m) where
  liftResourceT = lift . liftResourceT
