module Execute
  ( main
  ) where

import BasicPrelude hiding ( readFile )
import Control.Monad.Trans.Resource
import Data.Text.IO
import Data.Yaml hiding ( Parser )
import Network.AWS.Flow
import Options
import Options.Applicative

data Args = Args
  { aConfig :: FilePath
  , aPlan   :: FilePath
  , aInput  :: Maybe FilePath
  } deriving ( Eq, Read, Show )

args :: Parser Args
args = Args <$> configFile <*> planFile <*> inputFile

parser :: ParserInfo Args
parser =
  info ( helper <*> args ) $ fullDesc
    <> header   "execute: Execute a workflow"
    <> progDesc "Execute a workflow"

call :: Args -> IO ()
call Args{..} = do
  config <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
  plan <- decodeFile aPlan >>= maybeThrow (userError "Bad Plan")
  input <- readFileMaybe aInput
  env <- flowEnv config
  runResourceT $ runFlowT env $
    execute (strtTask $ plnStart plan) input where
      readFileMaybe =
        maybe (return Nothing) ((>>= return . Just) . readFile)

main :: IO ()
main = execParser parser >>= call
