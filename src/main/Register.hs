module Register
  ( main
  ) where

import Control.Monad.Trans.Resource hiding ( register )
import Data.Yaml
import Network.AWS.Flow
import Network.AWS.Flow.Env
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
main = do
  execParser argsPI >>= call where
    call Args{..} = do
      config <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
      plan <- decodeFile aPlan >>= maybeThrow (userError "Bad Plan")
      env <- flowEnv config
      runResourceT $ runFlowT env $
        register plan
