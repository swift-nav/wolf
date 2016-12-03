module Network.AWS.Flow.Logger
  ( logStrLn
  , logDebug'
  , logInfo'
  , logWarn'
  , logError'
  ) where

import Network.AWS.Flow.Prelude

import Control.Monad.Logger
import Data.Time.Clock
import Data.Time.Format
import Data.Version
import Formatting               hiding (now)
import Paths_wolf
import System.Log.FastLogger

prefix :: IO LogStr
prefix = do
  now <- getCurrentTime
  return $ toLogStr $ sformat (string % " name=wolf v=" % string % " ")
    (formatTime defaultTimeLocale "%FT%T%z" now)
    (showVersion version)

logStrLn :: LogStr -> IO ()
logStrLn s = do
  loggerSet <- newStderrLoggerSet defaultBufSize
  p <- prefix
  mapM_ (pushLogStr loggerSet) [p, s, "\n"]
  flushLogStr loggerSet

logDebug' :: MonadLogger m => Text -> m ()
logDebug' = logDebugN

logInfo' :: MonadLogger m => Text -> m ()
logInfo' = logInfoN

logWarn' :: MonadLogger m => Text -> m ()
logWarn' = logWarnN

logError' :: MonadLogger m => Text -> m ()
logError' = logErrorN
