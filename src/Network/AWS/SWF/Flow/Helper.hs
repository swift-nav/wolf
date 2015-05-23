{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.SWF.Flow.Helper where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.AWS
import Data.Aeson
import Network.AWS.SWF.Flow
import Network.HTTP.Conduit
import System.Log.FastLogger
import System.IO
import Data.UUID                  ( toString )
import Data.UUID.V4               ( nextRandom )
import Data.Text                  ( Text, pack )

data Config = Config
  { cRegion      :: Region
  , cCredentials :: Credentials
  , cTimeout     :: Int
  , cPollTimeout :: Int
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
    FromEnv <$>
      v .: "access-key-env-var" <*>
      v .: "secret-key-env-var"
  parseJSON _ = mzero

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
      v .: "region"       <*>
      v .: "credentials"  <*>
      v .: "timeout"      <*>
      v .: "poll-timeout"
  parseJSON _ = mzero

flowEnv :: Config -> IO FlowEnv
flowEnv Config{..} = do
  loggerSet <- newStderrLoggerSet defaultBufSize
  logger <- newLogger Info stderr
  manager <- newManager (managerSettings cTimeout)
  pollManager <- newManager (managerSettings cPollTimeout)
  env <- newEnv' manager <&> envLogger .~ logger
  pollEnv <- newEnv' pollManager <&> envLogger .~ logger
  return $ FlowEnv (logStrLn loggerSet) env pollEnv where
    managerSettings timeout =
      conduitManagerSettings { managerResponseTimeout = Just timeout }
    newEnv' m =
      runExceptT (newEnv cRegion cCredentials m) >>= either error return
    logStrLn ls s =
      pushLogStr ls s >> flushLogStr ls

newUid :: IO Text
newUid = do
  r <- nextRandom
  return $ pack $ toString r
