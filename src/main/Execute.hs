{-# LANGUAGE RecordWildCards #-}

module Execute ( main ) where

import Data.Text.IO            ( readFile )
import Data.Yaml               ( decodeFile )
import Network.AWS.Flow        ( Plan (..), Start (..), runFlowT, execute )
import Network.AWS.Flow.Helper ( flowEnv, newUid )
import Options.Applicative
import Prelude          hiding ( readFile )

data Args = Args
  { aConfig :: FilePath
  , aPlan   :: FilePath
  , aInput  :: FilePath
  } deriving ( Eq, Read, Show )

argsPI :: ParserInfo Args
argsPI =
  info ( helper <*> argsP )
    ( fullDesc
    <> header   "execute: Execute a workflow"
    <> progDesc "Execute a workflow" ) where
    argsP = args
      <$> strOption
          (  long    "config"
          <> short   'c'
          <> metavar "FILE"
          <> help    "AWS SWF Service Flow config" )
      <*> strOption
          ( long     "plan"
          <> short   'p'
          <> metavar "FILE"
          <> help    "AWS SWF Service Flow plan" )
      <*> strOption
          ( long     "input"
          <> short   'i'
          <> metavar "FILE"
          <> help    "AWS SWF Service Flow input" ) where
          args config plan input = Args
            { aConfig = config
            , aPlan   = plan
            , aInput  = input
            }

main :: IO ()
main =
  execParser argsPI >>= call where
    call Args{..} = do
      config <- decodeFile aConfig >>= hoistMaybe "Bad Config"
      plan <- decodeFile aPlan >>= hoistMaybe "Bad Plan"
      input <- readFile aInput
      env <- flowEnv config
      uid <- newUid
      r <- runFlowT env $
        execute uid (strtTask $ plnStart plan) (Just input)
      print r where
        hoistMaybe s =
          maybe (error s) return
