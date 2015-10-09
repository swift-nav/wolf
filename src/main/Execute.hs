module Execute
  ( main
  ) where

import Control.Monad.Trans.Resource
import Data.Text.IO
import Data.Yaml
import Network.AWS.Flow
import Network.AWS.Flow.Env
import Options.Applicative
import Prelude hiding ( readFile )

data Args = Args
  { aConfig :: FilePath
  , aPlan   :: FilePath
  , aInput  :: Maybe FilePath
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
      <*> optional ( strOption
          ( long     "input"
          <> short   'i'
          <> metavar "FILE"
          <> help    "AWS SWF Service Flow input" ) ) where
          args config plan input = Args
            { aConfig = config
            , aPlan   = plan
            , aInput  = input
            }

main :: IO ()
main =
  execParser argsPI >>= call where
    call Args{..} = do
      config <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
      plan <- decodeFile aPlan >>= maybeThrow (userError "Bad Plan")
      input <- readFileMaybe aInput
      env <- flowEnv config
      runResourceT $ runFlowT env $
        execute (strtTask $ plnStart plan) input where
          readFileMaybe =
            maybe (return Nothing) ((>>= return . Just) . readFile)
