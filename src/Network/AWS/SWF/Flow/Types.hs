{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}

module Network.AWS.SWF.Flow.Types where

import Control.Applicative         ( Applicative )
import Control.Monad.Catch         ( MonadCatch, MonadThrow )
import Control.Monad.IO.Class      ( MonadIO )
import Control.Monad.Except        ( ExceptT, MonadError )
import Control.Monad.Reader        ( ReaderT, MonadReader )
import Control.Monad.Trans.AWS     ( Credentials, Env, Error, LogLevel, Region )
import Control.Monad.Trans.Control ( MonadBaseControl )
import Data.Text                   ( Text )
import Network.AWS.SWF.Types       ( HistoryEvent )
import System.IO                   ( Handle )

type Domain   = Text
type Uid      = Text
type Name     = Text
type Version  = Text
type Queue    = Text
type Token    = Text
type Timeout  = Text
type Metadata = Maybe Text

data FlowConfig = FlowConfig
  { fcRegion      :: Region
  , fcCredentials :: Credentials
  , fcTimeout     :: Int
  , fcPollTimeout :: Int
  , fcLogLevel    :: LogLevel
  , fcLogHandle   :: Handle
  } deriving ( Eq )

data FlowEnv = FlowEnv
  { feUid     :: Uid
  , feEnv     :: Env
  , fePollEnv :: Env
  }

data FlowError
  = FlowError String
  | AWSError Error
  deriving ( Show )

newtype FlowT m a = FlowT
  { unFlowT :: ReaderT FlowEnv (ExceptT FlowError m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadCatch
             , MonadThrow
             , MonadError FlowError
             )

type MonadFlow m =
  ( MonadBaseControl IO m
  , MonadCatch m
  , MonadIO m
  , MonadReader FlowEnv m
  , MonadError FlowError m
  )

data DecideEnv = DecideEnv
  { deSpec      :: Spec
  , deUid       :: Uid
  , deEvents    :: [HistoryEvent]
  , deFindEvent :: (Integer -> Maybe HistoryEvent)
  }

newtype DecideT m a = DecideT
  { unDecideT :: ReaderT DecideEnv (ExceptT FlowError m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadCatch
             , MonadThrow
             , MonadError FlowError
             )

type MonadDecide m =
  ( MonadBaseControl IO m
  , MonadCatch m
  , MonadIO m
  , MonadReader DecideEnv m
  , MonadError FlowError m
  )

data Task = Task
  { tskName    :: Name
  , tskVersion :: Version
  , tskQueue   :: Queue
  } deriving ( Eq, Read, Show )

data Timer = Timer
  { tmrName    :: Text
  , tmrTimeout :: Text
  } deriving ( Eq, Read, Show )

data Spec
  = Start
  { strtTask :: Task
  , strtNext :: Spec
  }
  | Done
  | Continue
  | Work
  { wrkTask  :: Task
  , wrkNext  :: Spec
  }
  | Sleep
  { slpTimer :: Timer
  , slpNext  :: Spec
  } deriving ( Eq, Read, Show )
