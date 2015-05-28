{-# LANGUAGE RecordWildCards #-}

module Decide ( main ) where

import Data.Text                   ( pack )
import Data.Yaml                   ( decodeFile )
import Network.AWS.SWF.Flow        ( Domain, runFlowT, decide )
import Network.AWS.SWF.Flow.Helper ( flowEnv, newUid )
import Options.Applicative
import Prelude hiding              ( readFile )

data Args = Args
  { aDomain :: Domain
  , aConfig :: FilePath
  , aPlan   :: FilePath
  } deriving ( Eq, Read, Show )

argsPI :: ParserInfo Args
argsPI =
  info ( helper <*> argsP )
    ( fullDesc
    <> header   "decide: Decide a workflow"
    <> progDesc "Decide a workflow" ) where
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
          <> help    "AWS Simple Workflow Service Flow plan" ) where
          args domain config plan = Args
            { aDomain = pack domain
            , aConfig = config
            , aPlan   = plan
            }

main :: IO ()
main =
  execParser argsPI >>= call >>= print where
    call Args{..} = do
      config <- decodeFile aConfig >>= hoistMaybe "Bad Config"
      plan <- decodeFile aPlan >>= hoistMaybe "Bad Plan"
      env <- flowEnv config
      uid <- newUid
      runFlowT env $
        decide aDomain uid plan where
          hoistMaybe s a = maybe (error s) return a
