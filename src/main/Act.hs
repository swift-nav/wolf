{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Act ( main ) where

import Control.Exception          ( SomeException )
import Control.Monad              ( forever, forM, mzero, liftM )
import Control.Monad.IO.Class     ( MonadIO )
import Crypto.Hash                ( hash )
import Data.ByteString            ( length )
import Data.ByteString.Lazy       ( fromStrict )
import Data.Text                  ( Text, pack, append, words )
import Data.Yaml
import Network.AWS.Flow           ( Artifact, Metadata, Queue, Uid, runFlowT, act )
import Network.AWS.Flow.Env       ( flowEnv )
import Options.Applicative hiding ( action )
import Shelly              hiding ( FilePath )
import Prelude             hiding ( length, readFile, words, writeFile )

data Container = Container
  { cImage   :: Text
  , cCommand :: Text
  , cVolumes :: [Text]
  , cDevices :: [Text]
  } deriving ( Eq, Read, Show )

instance FromJSON Container where
  parseJSON (Object v) =
    Container              <$>
    v .:  "image"          <*>
    v .:  "command"        <*>
    v .:? "volumes" .!= [] <*>
    v .:? "devices" .!= []
  parseJSON _ = mzero

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

exec :: MonadIO m => Container -> Uid -> Metadata -> m (Metadata, [Artifact])
exec container uid metadata =
  shelly $ withDir $ \dataDir storeDir -> do
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
      input dir =
        maybe_ $ writefile $ dir </> pack "input.json" where
          maybe_ =
            maybe (return ())
      output dir =
        catch_sh_maybe $ readfile $ dir </> pack "output.json" where
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
      docker dataDir storeDir Container{..} =
        run_ "docker" $ concat
          [["run"]
          , devices
          , volumes
          , [cImage]
          , words cCommand
          ] where
            devices =
              concatMap (("--device" :) . return) cDevices
            volumes =
              concatMap (("--volume" :) . return) $
                append (toTextIgnore dataDir)  ":/app/data"  :
                append (toTextIgnore storeDir) ":/app/store" : cVolumes

main :: IO ()
main =
  execParser argsPI >>= call where
    call Args{..} = do
      config <- decodeFile aConfig >>= hoistMaybe "Bad Config"
      container <- decodeFile aContainer >>= hoistMaybe "Bad Container"
      env <- flowEnv config
      forever $ do
        r <- runFlowT env $
          act aQueue $ exec container
        print r where
          hoistMaybe s =
            maybe (error s) return
