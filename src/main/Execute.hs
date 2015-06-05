{-# LANGUAGE RecordWildCards #-}

module Execute ( main ) where

import Data.Text               ( pack )
import Data.Text.IO            ( readFile )
import Data.Yaml               ( decodeFile )
import Network.AWS.Flow        ( Domain, runFlowT, execute, plnStart, strtTask )
import Network.AWS.Flow.Helper ( flowEnv, newUid )
import Options.Applicative
import Prelude                 hiding ( readFile )

data Args = Args
  { aDomain :: Domain
  , aConfig :: FilePath
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
          (  long    "domain"
          <> short   'd'
          <> metavar "NAME"
          <> help    "AWS Simple Workflow Service domain" )
      <*> strOption
          (  long    "config"
          <> short   'c'
          <> metavar "FILE"
          <> help    "AWS Simple Workflow Service Flow config" )
      <*> strOption
          ( long     "plan"
          <> short   'p'
          <> metavar "FILE"
          <> help    "AWS Simple Workflow Service Flow plan" )
      <*> strOption
          ( long     "input"
          <> short   'i'
          <> metavar "FILE"
          <> help    "AWS Simple Workflow Service Flow input" ) where
          args domain config plan input = Args
            { aDomain = pack domain
            , aConfig = config
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
        execute aDomain uid (strtTask $ plnStart plan) (Just input)
      print r where
        hoistMaybe s =
          maybe (error s) return
