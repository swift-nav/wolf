{-# LANGUAGE RecordWildCards #-}

module Decide
  ( main
  ) where

import BasicPrelude
import Control.Concurrent.Async
import Control.Monad.Trans.Resource
import Data.Yaml                    hiding (Parser)
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
    <> header   "decide: Decide a workflow"
    <> progDesc "Decide a workflow"

decodePlans :: FilePath -> IO (Maybe [Plan])
decodePlans file = do
  plan <- decodeFile file
  plans <- decodeFile file
  return $ plans <|> pure <$> plan

call :: Args -> IO ()
call Args{..} = do
  config <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
  plans <- decodePlans aPlan >>= maybeThrow (userError "Bad Plan")
  env <- flowEnv config
  void $ runConcurrently $ sequenceA $ flip map plans $ \plan ->
    Concurrently $ forever $ runResourceT $ runFlowT env $ decide plan

main :: IO ()
main = execParser parser >>= call
