module Network.AWS.Flow.Env
  ( flowEnv
  ) where

import Control.Lens
import Control.Monad.Trans.AWS
import Network.AWS.Flow.Types
import System.Log.FastLogger
import System.IO

logStrLn :: LogStr -> IO ()
logStrLn s = do
  loggerSet <- newStderrLoggerSet defaultBufSize
  pushLogStr loggerSet s
  flushLogStr loggerSet

flowEnv :: FlowConfig -> IO FlowEnv
flowEnv FlowConfig{..} = do
  logger <- newLogger Info stderr
  env <- newEnv fcRegion fcCredentials <&> envLogger .~ logger
  return $ FlowEnv logStrLn env (fromIntegral fcTimeout) (fromIntegral fcPollTimeout) fcDomain fcBucket fcPrefix
