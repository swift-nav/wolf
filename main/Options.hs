module Options
  ( configFile
  , planFile
  , inputFile
  , containerFile
  , queue
  , containerless
  , gzip
  , logLevel
  ) where

import BasicPrelude
import Control.Monad.Logger
import Data.Text ( pack )
import Options.Applicative

configFile :: Parser String
configFile =
  strOption
    $  long    "config"
    <> short   'c'
    <> metavar "FILE"
    <> help    "AWS SWF Service Flow config"

planFile :: Parser String
planFile =
  strOption
    $  long    "plan"
    <> short   'p'
    <> metavar "FILE"
    <> help    "AWS SWF Service Flow plan"

inputFile :: Parser (Maybe String)
inputFile =
  optional $ strOption
    $ long     "input"
    <> short   'i'
    <> metavar "FILE"
    <> help    "AWS SWF Service Flow input"

containerFile :: Parser String
containerFile =
  strOption
    $  long    "container"
    <> short   'x'
    <> metavar "FILE"
    <> help    "AWS SWF Service Flow worker container"

queue :: Parser String
queue =
  strOption
    $  long    "queue"
    <> short   'q'
    <> metavar "NAME"
    <> help    "AWS SWF Service Flow queue"

containerless :: Parser (Maybe String)
containerless =
  optional $ strOption
    $  long    "containerless"
    <> metavar "DIR"
    <> help    "Run outside of container in directory"

gzip :: Parser Bool
gzip =
  switch
    $  long "gzip"
    <> help "GZIP contents of artifacts"

logLevel :: Parser LogLevel
logLevel =
  fmap ( toLogLevel . pack ) $
    strOption
      $  long    "log-level"
      <> metavar "LEVEL"
      <> help    "Minimum level of logging" where
        toLogLevel "debug" = LevelDebug
        toLogLevel "info"  = LevelInfo
        toLogLevel "warn"  = LevelWarn
        toLogLevel "error" = LevelError
        toLogLevel s       = LevelOther s
