{-# LANGUAGE ScopedTypeVariables #-}
module Decide
  ( main
  ) where

import BasicPrelude
import Control.Concurrent.Async
import Control.Monad.Trans.Resource
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
    <> header   "decide: Decide a workflow"
    <> progDesc "Decide a workflow"

call :: Args -> IO ()
call Args{..} = do
  config <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
  plan :: [Plan] <- decodeFile aPlan >>= maybeThrow (userError "Bad Plan")
  putStrLn "XXXXXXXXXXXXXXXXXX"
  print plan
  putStrLn "YYYYYYYYYYYYYYYYYY"
  env <- flowEnv config
  void $ runConcurrently $ sequenceA $ flip map plan $ \p ->
    Concurrently $ forever $ runResourceT $ runFlowT env $ decide p

main :: IO ()
main = execParser parser >>= call
