{-# LANGUAGE ScopedTypeVariables #-}

module Act
  ( main
  ) where

import           BasicPrelude hiding ( ByteString, (</>), hash, length, readFile )
import           Control.Monad.Trans.Resource
import           Data.Aeson.Encode
import           Data.ByteString ( length )
import qualified Data.ByteString.Lazy as BL
import           Data.Text ( pack, strip )
import           Data.Text.Lazy ( toStrict )
import           Data.Text.Lazy.Builder
import           Data.Yaml hiding ( Parser )
import           Network.AWS.Data.Crypto
import           Network.AWS.Flow
import           Options
import           Options.Applicative hiding ( action )
import           Shelly hiding ( FilePath, bash )

data Args = Args
  { aConfig        :: FilePath
  , aQueue         :: Queue
  , aContainer     :: FilePath
  , aContainerless :: Bool
  } deriving ( Eq, Read, Show )

args :: Parser Args
args = Args <$> configFile <*> (pack <$> queue) <*> containerFile <*> containerless

parser :: ParserInfo Args
parser =
  info ( helper <*> args ) $ fullDesc
    <> header   "act: Workflow activity"
    <> progDesc "Workflow activity"

data Container = Container
  { cImage       :: Text
  , cCommand     :: Text
  , cVolumes     :: [Text]
  , cDevices     :: [Text]
  , cEnvironment :: [Text]
  , cLink        :: [Text]
  } deriving ( Eq, Read, Show )

instance FromJSON Container where
  parseJSON (Object v) =
    Container                  <$>
    v .:  "image"              <*>
    v .:  "command"            <*>
    v .:? "volumes"     .!= [] <*>
    v .:? "devices"     .!= [] <*>
    v .:? "environment" .!= [] <*>
    v .:? "links"       .!= []
  parseJSON _ = mzero

data Control = Control
  { cUid :: Uid
  } deriving ( Eq, Read, Show )

instance ToJSON Control where
  toJSON Control{..} = object
    [ "run_uid" .= cUid
    ]

encodeText :: ToJSON a => a -> Text
encodeText = toStrict . toLazyText . encodeToTextBuilder . toJSON

exec :: MonadIO m => Container -> Bool -> Uid -> Metadata -> [Blob] -> m (Metadata, [Artifact])
exec container dockerless uid metadata blobs =
  shelly $ withDir $ \dir dataDir storeDir -> do
    control $ dataDir </> pack "control.json"
    storeInput $ storeDir </> pack "input"
    dataInput $ dataDir </> pack "input.json"
    if dockerless then
      bash dir container
    else
      docker dataDir storeDir container
    result <- dataOutput $ dataDir </> pack "output.json"
    artifacts <- storeOutput $ storeDir </> pack "output"
    return (result, artifacts) where
      withDir action =
        withTmpDir $ \dir -> do
          mkdir $ dir </> pack "data"
          mkdir $ dir </> pack "store"
          mkdir $ dir </> pack "store/input"
          mkdir $ dir </> pack "store/output"
          action dir (dir </> pack "data") (dir </> pack "store")
      control file =
        writefile file $ encodeText $ Control uid
      dataInput file =
        maybe (return ()) (writefile file) metadata
      dataOutput file =
        catch_sh_maybe (readfile file) where
          catch_sh_maybe action =
            catch_sh (liftM Just action) $ \(_ :: SomeException) -> return Nothing
      storeInput dir =
        forM_ blobs $ \(key, blob) -> do
          paths <- liftM strip $ run "dirname" [key]
          mkdir_p $ dir </> paths
          writeBinary (dir </> key) (BL.toStrict blob)
      storeOutput dir = do
        artifacts <- findWhen test_f dir
        forM artifacts $ \artifact -> do
          key <- relativeTo dir artifact
          blob <- readBinary artifact
          return ( toTextIgnore $ key
                 , hash blob
                 , fromIntegral $ length blob
                 , BL.fromStrict blob
                 )
      docker dataDir storeDir Container{..} = do
        devices <- forM cDevices $ \device ->
          liftM strip $ run "readlink" ["-f", device]
        run_ "docker" $ concat
          [["run"]
          , concatMap (("--device" :)  . return) devices
          , concatMap (("--env"    :)  . return) cEnvironment
          , concatMap (("--link"    :) . return) cLink
          , concatMap (("--volume" :)  . return) $
              toTextIgnore dataDir  <> ":/app/data"  :
              toTextIgnore storeDir <> ":/app/store" : cVolumes
          , [cImage]
          , words cCommand
          ]
      bash dir Container{..} = do
        cd dir
        run_ "bash" $ concat
          [["-c"]
          , words cCommand
          ]

call :: Args -> IO ()
call Args{..} = do
  config <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
  container <- decodeFile aContainer >>= maybeThrow (userError "Bad Container")
  env <- flowEnv config
  forever $ runResourceT $ runFlowT env $
    act aQueue $ exec container aContainerless

main :: IO ()
main = execParser parser >>= call
