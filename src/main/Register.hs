{-# LANGUAGE RecordWildCards #-}

module Register ( main ) where

import Data.Yaml               ( decodeFile )
import Network.AWS.Flow        ( runFlowT, register )
import Network.AWS.Flow.Helper ( flowEnv )
import Options.Applicative

data Args = Args
  { aConfig :: FilePath
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
          (  long    "config"
          <> short   'c'
          <> metavar "FILE"
          <> help    "AWS SWF Service Flow config" )
      <*> strOption
          ( long     "plan"
          <> short   'p'
          <> metavar "FILE"
          <> help    "AWS SWF Service Flow plan" ) where
          args config plan = Args
            { aConfig = config
            , aPlan   = plan
            }

main :: IO ()
main =
  execParser argsPI >>= call where
    call Args{..} = do
      config <- decodeFile aConfig >>= hoistMaybe "Bad Config"
      plan <- decodeFile aPlan >>= hoistMaybe "Bad Plan"
      env <- flowEnv config
      r <- runFlowT env $
        register plan
      print r where
        hoistMaybe s =
          maybe (error s) return
