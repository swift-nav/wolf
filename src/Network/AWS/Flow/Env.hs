{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.Flow.Env
  ( flowEnv
  ) where

import Control.Applicative     ( (<$>), (<*>) )
import Control.Lens            ( (.~), (<&>) )
import Control.Monad           ( mzero )
import Control.Monad.Except    ( runExceptT )
import Control.Monad.Trans.AWS
import Data.Aeson
import Network.AWS.Flow
import Network.HTTP.Conduit    ( managerResponseTimeout
                               , newManager
                               , tlsManagerSettings )
import System.Log.FastLogger   ( defaultBufSize
                               , flushLogStr
                               , newStderrLoggerSet
                               , pushLogStr )
import System.IO               ( stderr )

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
      v .: "secret-key-env-var"
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

flowEnv :: FlowConfig -> IO FlowEnv
flowEnv FlowConfig{..} = do
  loggerSet <- newStderrLoggerSet defaultBufSize
  logger <- newLogger Info stderr
  manager <- newManager (managerSettings fcTimeout)
  pollManager <- newManager (managerSettings fcPollTimeout)
  env <- newEnv' manager <&> envLogger .~ logger
  pollEnv <- newEnv' pollManager <&> envLogger .~ logger
  return $ FlowEnv (logStrLn loggerSet) env pollEnv fcDomain fcBucket fcPrefix where
    managerSettings timeout =
      tlsManagerSettings { managerResponseTimeout = Just timeout }
    newEnv' m =
      runExceptT (newEnv fcRegion fcCredentials m) >>= either error return
    logStrLn ls s =
      pushLogStr ls s >> flushLogStr ls
