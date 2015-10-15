module Decide
  ( main
  ) where

import Control.Monad
import Control.Monad.Trans.Resource
import Data.Yaml
import Network.AWS.Flow
import Options.Applicative
import Prelude hiding ( readFile )

data Args = Args
  { aConfig :: FilePath
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
      config <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
      plan <- decodeFile aPlan >>= maybeThrow (userError "Bad Plan")
      env <- flowEnv config
      forever $
        runResourceT $ runFlowT env $
          decide plan
