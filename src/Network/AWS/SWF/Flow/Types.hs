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

data Config = Config
  { cfgRegion      :: Region
  , cfgCredentials :: Credentials
  , cfgTimeout     :: Int
  , cfgPollTimeout :: Int
  , cfgLogLevel    :: LogLevel
  , cfgLogHandle   :: Handle
  } deriving ( Eq )

data Context = Context
  { ctxUid     :: Uid
  , ctxEnv     :: Env
  , ctxPollEnv :: Env
  }

data FlowError
  = FlowError String
  | AWSError Error
  deriving ( Show )

newtype FlowT m a = FlowT
  { unFlowT :: ReaderT Context (ExceptT FlowError m) a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadError FlowError )

type MonadFlow m =
  ( MonadBaseControl IO m
  , MonadCatch m
  , MonadIO m
  , MonadReader Context m
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

data Store = Store
  { strSpec      :: Spec
  , strUid       :: Uid
  , strEvents    :: [HistoryEvent]
  , strFindEvent :: (Integer -> Maybe HistoryEvent)
  }

newtype ChoiceT m a = ChoiceT
  { unChoiceT :: ReaderT Store (ExceptT FlowError m) a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadError FlowError )

type MonadChoice m =
  ( MonadBaseControl IO m
  , MonadCatch m
  , MonadIO m
  , MonadReader Store m
  , MonadError FlowError m
  )

