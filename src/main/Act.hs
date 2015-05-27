{-# LANGUAGE RecordWildCards #-}

module Act ( main ) where

import Data.Text                   ( pack )
import Data.Yaml                   ( decodeFile )
import Network.AWS.SWF.Flow        ( Domain  )
import Network.AWS.SWF.Flow.Helper ( flowEnv )
import Options.Applicative

data Args = Args
  { aDomain    :: Domain
  , aConfig    :: FilePath
  , aContainer :: String
  , aParams    :: [String]
  } deriving ( Eq, Read, Show )

argsPI :: ParserInfo Args
argsPI =
  info ( helper <*> argsP )
    ( fullDesc
    <> header   "act: Workflow activity"
    <> progDesc "Workflow activity" ) where
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
          (  long    "container"
          <> short   'x'
          <> metavar "NAME"
          <> help    "AWS Simple Workflow Service Flow worker container" )
      <*> many
          ( argument str
          (  metavar "PARAMS"
          <> help    "AWS Simple Workflow Service Flow worker container parameters" ) ) where
          args domain config container params = Args
            { aDomain    = pack domain
            , aConfig    = config
            , aContainer = container
            , aParams    = params
            }

main :: IO ()
main =
  execParser argsPI >>= run >>= print where
    run Args{..} = do
      config <- decodeFile aConfig >>= hoistMaybe "Bad Config"
      env <- flowEnv config
      return () where
        hoistMaybe s a = maybe (error s) return a
