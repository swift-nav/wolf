module Network.AWS.Flow.Env
  ( flowEnv
  ) where

import Network.AWS.Flow.Logger
import Network.AWS.Flow.Prelude
import Network.AWS.Flow.Types

import System.IO

flowEnv :: FlowConfig -> IO FlowEnv
flowEnv FlowConfig{..} = do
  logger <- newLogger Info stderr
  env <- newEnv fcRegion fcCredentials <&> envLogger .~ logger
  return $ FlowEnv logStrLn env (fromIntegral fcTimeout) (fromIntegral fcPollTimeout) fcDomain fcBucket fcPrefix
