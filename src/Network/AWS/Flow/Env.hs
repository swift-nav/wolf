module Network.AWS.Flow.Env
  ( flowEnv
  ) where

import Control.Lens
import Control.Monad.Trans.AWS
import Network.AWS.Flow.Types
import System.Log.FastLogger
import System.IO

flowEnv :: FlowConfig -> IO FlowEnv
flowEnv FlowConfig{..} = do
  loggerSet <- newStderrLoggerSet defaultBufSize
  logger <- newLogger Info stderr
  env <- newEnv fcRegion fcCredentials <&> envLogger .~ logger
  return $ FlowEnv (logStrLn loggerSet) env (fromIntegral fcTimeout) (fromIntegral fcPollTimeout) fcDomain fcBucket fcPrefix where
    logStrLn ls s =
      pushLogStr ls s >> flushLogStr ls
