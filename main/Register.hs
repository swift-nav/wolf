module Register
  ( main
  ) where

import BasicPrelude
import Control.Monad.Trans.Resource hiding ( register )
import Data.Yaml hiding ( Parser )
import Network.AWS.Flow
import Options
import Options.Applicative

data Args = Args
  { aConfig :: FilePath
  , aPlan   :: FilePath
  } deriving ( Eq, Read, Show )

args :: Parser Args
args = Args <$> configFile <*> planFile

parser :: ParserInfo Args
parser =
  info ( helper <*> args ) $ fullDesc
    <> header   "register: Register a workflow"
    <> progDesc "Register a workflow"

call :: Args -> IO ()
call Args{..} = do
  config' <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
  plan <- decodeFile aPlan >>= maybeThrow (userError "Bad Plan")
  env <- flowEnv config'
  runResourceT $ runFlowT env $ register plan

main :: IO ()
main = execParser parser >>= call
