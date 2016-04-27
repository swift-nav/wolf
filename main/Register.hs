module Register
  ( main
  ) where

import BasicPrelude
import Control.Concurrent.Async
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

decodePlans :: FilePath -> IO (Maybe [Plan])
decodePlans file = do
  plan <- decodeFile file
  plans <- decodeFile file
  return $ plans <|> pure <$> plan

call :: Args -> IO ()
call Args{..} = do
  config' <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
  plans <- decodePlans aPlan >>= maybeThrow (userError "Bad Plan")
  env <- flowEnv config'
  void $ runConcurrently $ sequenceA $ flip map plans $ \plan ->
    Concurrently $ runResourceT $ runFlowT env $ register plan

main :: IO ()
main = execParser parser >>= call
