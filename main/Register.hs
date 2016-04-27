{-# LANGUAGE ScopedTypeVariables #-}
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

call :: Args -> IO ()
call Args{..} = do
  config' <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
  plan :: [Plan] <- decodeFile aPlan >>= maybeThrow (userError "Bad Plan")
  env <- flowEnv config'
  void $ runConcurrently $ sequenceA $ flip map plan $ \p ->
    Concurrently $ runResourceT $ runFlowT env $ register p

main :: IO ()
main = execParser parser >>= call
