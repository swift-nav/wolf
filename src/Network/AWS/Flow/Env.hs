module Network.AWS.Flow.Env
  ( flowEnv
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.AWS
import Data.Aeson
import Network.AWS.Flow.Types
import System.Log.FastLogger
import System.IO

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

flowEnv :: FlowConfig -> IO FlowEnv
flowEnv FlowConfig{..} = do
  loggerSet <- newStderrLoggerSet defaultBufSize
  logger <- newLogger Info stderr
  env <- newEnv fcRegion fcCredentials <&> envLogger .~ logger
  return $ FlowEnv (logStrLn loggerSet) env (fromIntegral fcTimeout) (fromIntegral fcPollTimeout) fcDomain fcBucket fcPrefix where
    logStrLn ls s =
      pushLogStr ls s >> flushLogStr ls
