{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

module Network.AWS.Flow.Types where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Trans.AWS
import Data.Aeson
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

instance FromJSON Region where
  parseJSON (String v)
    | v == "eu-west-1"          = return Ireland
    | v == "eu-central-1"       = return Frankfurt
    | v == "ap-northeast-1"     = return Tokyo
    | v == "ap-southeast-1"     = return Singapore
    | v == "ap-southeast-2"     = return Sydney
    | v == "cn-north-1"         = return Beijing
    | v == "us-east-1"          = return NorthVirginia
    | v == "us-west-1"          = return NorthCalifornia
    | v == "us-west-2"          = return Oregon
    | v == "us-gov-west-1"      = return GovCloud
    | v == "fips-us-gov-west-1" = return GovCloudFIPS
    | v == "sa-east-1"          = return SaoPaulo
    | otherwise = mzero
  parseJSON _ = mzero

instance FromJSON Credentials where
  parseJSON (Object v) =
    FromEnv                     <$>
      v .: "access-key-env-var" <*>
      v .: "secret-key-env-var" <*>
      pure Nothing
  parseJSON _ = mzero

instance FromJSON FlowConfig where
  parseJSON (Object v) =
    FlowConfig            <$>
      v .: "region"       <*>
      v .: "credentials"  <*>
      v .: "timeout"      <*>
      v .: "poll-timeout" <*>
      v .: "domain"       <*>
      v .: "bucket"       <*>
      v .: "prefix"
  parseJSON _ = mzero

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
  { unFlowT :: LoggingT (AWST' FlowEnv m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadLogger
             , MonadActive
             )

type MonadFlow m =
  ( MonadCatch m
  , MonadThrow m
  , MonadResource m
  , MonadLogger m
  , MonadReader FlowEnv m
  )

instance MonadThrow m => MonadThrow (FlowT m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (FlowT m) where
    catch (FlowT m) f = FlowT (catch m (unFlowT . f))

instance MonadBase b m => MonadBase b (FlowT m) where
    liftBase = liftBaseDefault

instance MonadTrans FlowT where
  lift = FlowT . lift . lift

instance MonadTransControl FlowT where
    type StT FlowT a = StT (ReaderT FlowEnv) a

    liftWith f = FlowT $
      liftWith $ \g ->
        liftWith $ \h ->
          f (h . g . unFlowT)

    restoreT = FlowT . restoreT . restoreT

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

instance HasEnv FlowEnv where
  environment = lens feEnv (\s a -> s { feEnv = a })

runFlowT :: FlowEnv -> FlowT m a -> m a
runFlowT e (FlowT m) = runAWST e (runLoggingT m (const . const . const $ feLogger e))

data DecideEnv = DecideEnv
  { deLogger    :: Log
  , dePlan      :: Plan
  , deEvents    :: [HistoryEvent]
  , deFindEvent :: Integer -> Maybe HistoryEvent
  }

newtype DecideT m a = DecideT
  { unDecideT :: LoggingT (ReaderT DecideEnv m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadLogger
             , MonadActive
             )

type MonadDecide m =
  ( MonadCatch m
  , MonadThrow m
  , MonadResource m
  , MonadLogger m
  , MonadReader DecideEnv m
  )

instance MonadThrow m => MonadThrow (DecideT m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (DecideT m) where
    catch (DecideT m) f = DecideT (catch m (unDecideT . f))

instance MonadBase b m => MonadBase b (DecideT m) where
    liftBase = liftBaseDefault

instance MonadTrans DecideT where
  lift = DecideT . lift . lift

instance MonadTransControl DecideT where
    type StT DecideT a = StT (ReaderT DecideEnv) a

    liftWith f = DecideT $
      liftWith $ \g ->
        liftWith $ \h ->
          f (h . g . unDecideT)

    restoreT = DecideT . restoreT . restoreT

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

runDecideT :: DecideEnv -> DecideT m a -> m a
runDecideT e (DecideT m) = runReaderT (runLoggingT m (const . const . const $ deLogger e)) e

data Task = Task
  { tskName    :: Name
  , tskVersion :: Version
  , tskQueue   :: Queue
  , tskTimeout :: Timeout
  } deriving ( Eq, Read, Show )

instance FromJSON Task where
  parseJSON (Object v) =
    Task             <$>
      v .: "name"    <*>
      v .: "version" <*>
      v .: "queue"   <*>
      v .: "timeout"
  parseJSON _ = mzero

data Timer = Timer
  { tmrName    :: Name
  , tmrTimeout :: Timeout
  } deriving ( Eq, Read, Show )

instance FromJSON Timer where
  parseJSON (Object v) =
    Timer            <$>
      v .: "name"    <*>
      v .: "timeout"
  parseJSON _ = mzero

data Start = Start
  { strtTask :: Task
  } deriving ( Eq, Read, Show )

instance FromJSON Start where
  parseJSON (Object v) =
    Start         <$>
      v .: "flow"
  parseJSON _ = mzero

data Spec
  = Work
  { wrkTask :: Task
  }
  | Sleep
  { slpTimer :: Timer
  } deriving ( Eq, Read, Show )

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

data End
  = Stop
  | Continue
  deriving ( Eq, Read, Show )

instance FromJSON End where
  parseJSON (String v)
    | v == "stop"     = return Stop
    | v == "continue" = return Continue
    | otherwise = mzero
  parseJSON _ = mzero

data Plan = Plan
  { plnStart :: Start
  , plnSpecs :: [Spec]
  , plnEnd   :: End
  } deriving ( Eq, Read, Show )

instance FromJSON Plan where
  parseJSON (Object v) =
    Plan           <$>
      v .: "start" <*>
      v .: "specs" <*>
      v .: "end"
  parseJSON _ = mzero

