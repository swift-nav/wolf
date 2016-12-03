{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Act
  ( main
  ) where

import           BasicPrelude                 hiding (ByteString, find, hash, length,
                                               readFile, (<.>), (</>))
import           Codec.Compression.GZip
import           Control.Monad.Trans.Resource
import           Data.Aeson.Encode
import           Data.ByteString              (length)
import qualified Data.ByteString.Lazy         as BL
import           Data.Text                    (pack, strip)
import           Data.Text.Lazy               (toStrict)
import           Data.Text.Lazy.Builder       hiding (fromText)
import           Data.Yaml                    hiding (Parser)
import           Filesystem.Path              (dropExtension, (<.>))
import           Network.AWS.Data.Crypto
import           Network.AWS.Flow
import           Options
import           Options.Applicative          hiding (action)
import           Shelly                       hiding (FilePath, bash, (<.>))

data Args = Args
  { aConfig        :: FilePath
  , aQueue         :: Queue
  , aContainer     :: FilePath
  , aContainerless :: Maybe String
  , aGzip          :: Bool
  } deriving ( Eq, Read, Show )

args :: Parser Args
args = Args        <$>
  configFile       <*>
  (pack <$> queue) <*>
  containerFile    <*>
  containerless    <*>
  gzip

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

handler :: MonadBaseControl IO m => m () -> m (Maybe SomeException)
handler a = handle (return . Just) $ a >> return Nothing

exec :: MonadIO m => Args -> Container -> Uid -> Metadata -> [Blob] -> m (Metadata, [Artifact], Maybe SomeException)
exec Args{..} container uid metadata blobs =
  shelly $ withDir $ \dir dataDir storeDir -> do
    control $ dataDir </> pack "control.json"
    storeInput $ storeDir </> pack "input"
    dataInput $ dataDir </> pack "input.json"
    e <- maybe (docker dataDir storeDir container) (bash dir container) aContainerless
    result <- dataOutput $ dataDir </> pack "output.json"
    artifacts <- storeOutput $ storeDir </> pack "output"
    return (result, artifacts, e) where
      withDir action =
        withTmpDir $ \dir -> do
          mkdir $ dir </> pack "data"
          mkdir $ dir </> pack "store"
          mkdir $ dir </> pack "store/input"
          mkdir $ dir </> pack "store/output"
          action dir (dir </> pack "data") (dir </> pack "store")
      control file =
        writefile file $ encodeText $ Control uid
      writeArtifact file blob =
        if aGzip then
          writeBinary (dropExtension file) $ BL.toStrict $ decompress blob
        else
          writeBinary file $ BL.toStrict blob
      readArtifact dir file =
        if aGzip then do
          key <- relativeTo dir file
          blob <- BL.toStrict . compress . BL.fromStrict <$> readBinary file
          return ( toTextIgnore (key <.> "gz")
                 , hash blob
                 , fromIntegral $ length blob
                 , BL.fromStrict blob
                 )
          else do
            key <- relativeTo dir file
            blob <- readBinary file
            return ( toTextIgnore key
                   , hash blob
                   , fromIntegral $ length blob
                   , BL.fromStrict blob
                   )
      dataInput file =
        maybe (return ()) (writefile file) metadata
      dataOutput file =
        catch_sh_maybe (readfile file) where
          catch_sh_maybe action =
            catch_sh (Just <$> action) $ \(_ :: SomeException) -> return Nothing
      storeInput dir =
        forM_ blobs $ \(key, blob) -> do
          paths <- strip <$> run "dirname" [key]
          mkdir_p $ dir </> paths
          writeArtifact (dir </> key) blob
      storeOutput dir = do
        artifacts <- findWhen test_f dir
        forM artifacts $ readArtifact dir
      docker dataDir storeDir Container{..} =
        handler $ do
          devices <- forM cDevices $ \device ->
            strip <$> run "readlink" ["-f", device]
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
      bash dir Container{..} bashDir =
        handler $ do
          files <- ls $ fromText $ pack bashDir
          forM_ files $ flip cp_r dir
          cd dir
          maybe (return ()) (uncurry $ run_ . fromText) $ uncons $ words cCommand

call :: Args -> IO ()
call Args{..} = do
  config <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
  container <- decodeFile aContainer >>= maybeThrow (userError "Bad Container")
  env <- flowEnv config
  forever $ runResourceT $ runFlowT env $
    act aQueue $ exec Args{..} container

main :: IO ()
main = execParser parser >>= call
