{-# LANGUAGE RecordWildCards #-}

module Register ( main ) where

import Data.Text                   ( pack )
import Data.Yaml                   ( decodeFile )
import Network.AWS.SWF.Flow        ( Domain, runFlowT, register )
import Network.AWS.SWF.Flow.Helper ( flowEnv )
import Options.Applicative

data Args = Args
  { aDomain :: Domain
  , aConfig :: FilePath
  , aPlan   :: FilePath
  } deriving ( Eq, Read, Show )

argsPI :: ParserInfo Args
argsPI =
  info ( helper <*> argsP )
    ( fullDesc
    <> header   "register: Register a workflow"
    <> progDesc "Register a workflow" ) where
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
            { aDomain   = pack domain
            , aConfig = config
            , aPlan = plan
            }

main :: IO ()
main =
  execParser argsPI >>= call where
    call Args{..} = do
      config <- decodeFile aConfig >>= hoistMaybe "Bad Config"
      plan <- decodeFile aPlan >>= hoistMaybe "Bad Plan"
      env <- flowEnv config
      r <- runFlowT env $
        register aDomain plan
      print r where
        hoistMaybe s =
          maybe (error s) return
