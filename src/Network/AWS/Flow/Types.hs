{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}

module Network.AWS.Flow.Types where

import Control.Applicative         ( Applicative )
import Control.Monad.Catch         ( MonadCatch, MonadThrow )
import Control.Monad.IO.Class      ( MonadIO )
import Control.Monad.Except        ( ExceptT, MonadError )
import Control.Monad.Logger        ( LoggingT, MonadLogger, LogStr )
import Control.Monad.Reader        ( ReaderT, MonadReader )
import Control.Monad.Trans.AWS     ( Credentials, Env, Error, Region )
import Control.Monad.Trans.Control ( MonadBaseControl )
import Data.Text                   ( Text )
import Network.AWS.SWF.Types       ( HistoryEvent )

type Uid      = Text
type Name     = Text
type Version  = Text
type Queue    = Text
type Token    = Text
type Timeout  = Text
type Key      = Text
type Metadata = Maybe Text

data FlowConfig = FlowConfig
  { fcRegion      :: Region
  , fcCredentials :: Credentials
  , fcTimeout     :: Int
  , fcPollTimeout :: Int
  , fcDomain      :: Text
  , fcBucket      :: Text
  }

data FlowEnv = FlowEnv
  { feLogger  :: LogStr -> IO ()
  , feEnv     :: Env
  , fePollEnv :: Env
  , feDomain  :: Text
  , feBucket  :: Text
  }

data FlowError
  = FlowError String
  | AWSError Error
  deriving ( Show )

newtype FlowT m a = FlowT
  { unFlowT :: LoggingT (ReaderT FlowEnv (ExceptT FlowError m)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadLogger
             , MonadCatch
             , MonadThrow
             , MonadError FlowError
             )

type MonadFlow m =
  ( MonadBaseControl IO m
  , MonadCatch m
  , MonadIO m
  , MonadLogger m
  , MonadReader FlowEnv m
  , MonadError FlowError m
  )

data DecideEnv = DecideEnv
  { deLogger    :: LogStr -> IO ()
  , deUid       :: Uid
  , dePlan      :: Plan
  , deEvents    :: [HistoryEvent]
  , deFindEvent :: Integer -> Maybe HistoryEvent
  }

newtype DecideT m a = DecideT
  { unDecideT :: LoggingT (ReaderT DecideEnv (ExceptT FlowError m)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadLogger
             , MonadCatch
             , MonadThrow
             , MonadError FlowError
             )

type MonadDecide m =
  ( MonadBaseControl IO m
  , MonadCatch m
  , MonadIO m
  , MonadLogger m
  , MonadReader DecideEnv m
  , MonadError FlowError m
  )

data Task = Task
  { tskName    :: Name
  , tskVersion :: Version
  , tskQueue   :: Queue
  , tskTimeout :: Timeout
  } deriving ( Eq, Read, Show )

data Timer = Timer
  { tmrName    :: Name
  , tmrTimeout :: Timeout
  } deriving ( Eq, Read, Show )

data Start = Start
  { strtTask :: Task
  } deriving ( Eq, Read, Show )

data Spec
  = Work
  { wrkTask :: Task
  }
  | Sleep
  { slpTimer :: Timer
  } deriving ( Eq, Read, Show )

data End
  = Stop
  | Continue
  deriving ( Eq, Read, Show )

data Plan = Plan
  { plnStart :: Start
  , plnSpecs :: [Spec]
  , plnEnd   :: End
  } deriving ( Eq, Read, Show )
