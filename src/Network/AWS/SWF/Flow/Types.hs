module Network.AWS.SWF.Flow.Types where

import Control.Monad.Trans.AWS ( Credentials, Env, Error, LogLevel, Region )
import Data.Text               ( Text )
import System.IO               ( Handle )


type EitherE = Either Error

data Config = Config
  { cfgRegion      :: Region
  , cfgCredentials :: Credentials
  , cfgTimeout     :: Int
  , cfgPollTimeout :: Int
  , cfgLogLevel    :: LogLevel
  , cfgLogHandle   :: Handle
  } deriving ( Eq )

data Context = Context
  { ctxUid     :: Text
  , ctxEnv     :: Env
  , ctxPollEnv :: Env
  }

data Task = Task
  { tskName    :: Text
  , tskVersion :: Text
  , tskList    :: Text
  } deriving ( Eq, Read, Show )

data Timer = Timer
  { tmrName    :: Text
  , tmrTimeout :: Text
  } deriving ( Eq, Read, Show )

data Spec = Start
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
