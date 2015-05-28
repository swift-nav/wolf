{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Act ( main ) where

import Control.Monad               ( mzero )
import Data.Text                   ( Text, pack, append )
import Data.Yaml
import Network.AWS.SWF.Flow        ( Domain, Queue, Metadata, runFlowT, act )
import Network.AWS.SWF.Flow.Helper ( flowEnv, newUid )
import Options.Applicative  hiding ( action )
import Shelly                      ( (</>), run_, shelly, withTmpDir, toTextIgnore, writefile, readfile )
import Prelude              hiding ( readFile, writeFile )
import Control.Monad.IO.Class      ( MonadIO )

data Container = Container
  { cImage :: Text
  , cOpts  :: [Text]
  , cArgs  :: [Text]
  } deriving ( Eq, Read, Show )

instance FromJSON Container where
  parseJSON (Object v) =
    Container        <$>
    v .: "image"     <*>
    v .: "options"   <*>
    v .: "arguments"
  parseJSON _ = mzero

data Args = Args
  { aDomain    :: Domain
  , aConfig    :: FilePath
  , aQueue     :: Queue
  , aContainer :: FilePath
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
          ( long     "queue"
          <> short   'q'
          <> metavar "NAME"
          <> help    "AWS Simple Workflow Service Flow queue" )
      <*> strOption
          (  long    "container"
          <> short   'x'
          <> metavar "FILE"
          <> help    "AWS Simple Workflow Service Flow worker container" ) where
        args domain config queue container = Args
          { aDomain    = pack domain
          , aConfig    = config
          , aQueue     = pack queue
          , aContainer = container
          }

exec :: MonadIO m => Text -> Container -> m Text
exec metadata container =
  shelly $ withTmpDir $ \dir -> do
    writefile (dir </> pack "input.json") metadata
    docker (toTextIgnore dir) container
    readfile (dir </> pack "output.json") where
      docker dir Container{..} =
        run_ "docker" $ concat
          [["run"]
          , concat
              [["-v"
              , append dir ":/app/data"]
              , cOpts
              ]
          , [cImage]
          , cArgs
          ]

handle :: Monad m => Metadata -> m Metadata
handle a = return a

main :: IO ()
main =
  execParser argsPI >>= run >>= print where
    run Args{..} = do
      config <- decodeFile aConfig >>= hoistMaybe "Bad Config"
      env <- flowEnv config
      uid <- newUid
      runFlowT env $
        act aDomain uid aQueue handle where
          hoistMaybe s a = maybe (error s) return a
