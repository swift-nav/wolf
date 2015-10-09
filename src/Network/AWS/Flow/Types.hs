{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}

module Network.AWS.Flow.Types where

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Trans.AWS
import Data.ByteString.Lazy
import Data.Conduit.Lazy
import Data.Text
import Network.AWS.Data.Crypto
import Network.AWS.SWF.Types

type Uid      = Text
type Name     = Text
type Version  = Text
type Queue    = Text
type Token    = Text
type Timeout  = Text
type Metadata = Maybe Text
type Artifact = (Text, Digest SHA256, Integer, ByteString)
type Log      = LogStr -> IO ()

data FlowConfig = FlowConfig
  { fcRegion      :: Region
  , fcCredentials :: Credentials
  , fcTimeout     :: Int
  , fcPollTimeout :: Int
  , fcDomain      :: Text
  , fcBucket      :: Text
  , fcPrefix      :: Text
  }

data FlowEnv = FlowEnv
  { feLogger      :: Log
  , feEnv         :: Env
  , feTimeout     :: Seconds
  , fePollTimeout :: Seconds
  , feDomain      :: Text
  , feBucket      :: Text
  , fePrefix      :: Text
  }

newtype FlowT m a = FlowT
  { unFlowT :: ReaderT FlowEnv m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadActive
             , MonadTrans
             )

type MonadFlow m =
  ( MonadThrow m
  , MonadCatch m
  , MonadResource m
  , MonadReader FlowEnv m
  )

data DecideEnv = DecideEnv
  { deLogger    :: Log
  , dePlan      :: Plan
  , deEvents    :: [HistoryEvent]
  , deFindEvent :: Integer -> Maybe HistoryEvent
  }

newtype DecideT m a = DecideT
  { unDecideT :: ReaderT DecideEnv m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadActive
             , MonadTrans
             )

type MonadDecide m =
  ( MonadCatch m
  , MonadResource m
  , MonadReader DecideEnv m
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
