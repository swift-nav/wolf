{-# LANGUAGE ScopedTypeVariables #-}

module Act
  ( main
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Aeson.Encode
import Data.ByteString ( length )
import Data.ByteString.Lazy ( fromStrict )
import Data.Text ( Text, pack, words, strip )
import Data.Text.Lazy ( toStrict )
import Data.Text.Lazy.Builder
import Data.Yaml
import Network.AWS.Data.Crypto
import Network.AWS.Flow
import Options.Applicative hiding ( action )
import Shelly hiding ( FilePath )
import Prelude hiding ( length, readFile, words, writeFile )

data Args = Args
  { aConfig    :: FilePath
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
          (  long    "config"
          <> short   'c'
          <> metavar "FILE"
          <> help    "AWS SWF Service Flow config" )
      <*> strOption
          ( long     "queue"
          <> short   'q'
          <> metavar "NAME"
          <> help    "AWS SWF Service Flow queue" )
      <*> strOption
          (  long    "container"
          <> short   'x'
          <> metavar "FILE"
          <> help    "AWS SWF Service Flow worker container" ) where
        args config queue container = Args
          { aConfig    = config
          , aQueue     = pack queue
          , aContainer = container
          }

data Container = Container
  { cImage       :: Text
  , cCommand     :: Text
  , cVolumes     :: [Text]
  , cDevices     :: [Text]
  , cEnvironment :: [Text]
  } deriving ( Eq, Read, Show )

instance FromJSON Container where
  parseJSON (Object v) =
    Container                  <$>
    v .:  "image"              <*>
    v .:  "command"            <*>
    v .:? "volumes"     .!= [] <*>
    v .:? "devices"     .!= [] <*>
    v .:? "environment" .!= []
  parseJSON _ = mzero

data Control = Control
  { cUid :: Uid
  } deriving ( Eq, Read, Show )

instance ToJSON Control where
  toJSON Control{..} = object
    [ "run_uid" .= cUid
    ]

encodeText :: ToJSON a => a -> Text
encodeText =
  toStrict . toLazyText . encodeToTextBuilder . toJSON

exec :: MonadIO m => Container -> Uid -> Metadata -> m (Metadata, [Artifact])
exec container uid metadata =
  shelly $ withDir $ \dataDir storeDir -> do
    control dataDir $ encodeText $ Control uid
    input dataDir metadata
    docker dataDir storeDir container
    result <- output dataDir
    artifacts <- store storeDir
    return (result, artifacts) where
      withDir action =
        withTmpDir $ \dir -> do
          mkdir $ dir </> pack "data"
          mkdir $ dir </> pack "store"
          action (dir </> pack "data") (dir </> pack "store")
      control dir =
        writefile (dir </> pack "control.json")
      input dir =
        maybe_ (writefile $ dir </> pack "input.json") where
          maybe_ =
            maybe (return ())
      output dir =
        catch_sh_maybe (readfile $ dir </> pack "output.json") where
          catch_sh_maybe action =
            catch_sh (liftM Just action) $ \(_ :: SomeException) -> return Nothing
      store dir = do
        artifacts <- findWhen test_f dir
        forM artifacts $ \artifact -> do
          key <- relativeTo dir artifact
          blob <- readBinary artifact
          return ( toTextIgnore $ uid </> key
                 , hash blob
                 , fromIntegral $ length blob
                 , fromStrict blob
                 )
      docker dataDir storeDir Container{..} = do
        devices <- forM cDevices $ \device ->
          liftM strip $ run "readlink" ["-f", device]
        run_ "docker" $ concat
          [["run"]
          , concatMap (("--device" :) . return) devices
          , concatMap (("--env"    :) . return) cEnvironment
          , concatMap (("--volume" :) . return) $
              (toTextIgnore dataDir)  <> ":/app/data"  :
              (toTextIgnore storeDir) <> ":/app/store" : cVolumes
          , [cImage]
          , words cCommand
          ]

main :: IO ()
main =
  execParser argsPI >>= call where
    call Args{..} = do
      config <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
      container <- decodeFile aContainer >>= maybeThrow (userError "Bad Container")
      env <- flowEnv config
      forever $
        runResourceT $ runFlowT env $
          act aQueue $ exec container
