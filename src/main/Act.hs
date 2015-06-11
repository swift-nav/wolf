{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Act ( main ) where

import Control.Exception          ( SomeException )
import Control.Monad              ( forever, forM_, mzero, liftM )
import Control.Monad.IO.Class     ( MonadIO )
import Data.Text                  ( Text, pack, append, words, unpack )
import Data.Yaml
import Network.AWS.Flow           ( Domain, Metadata, Pail, Queue, runFlowT, act, putter )
import Network.AWS.Flow.Helper    ( flowEnv, newUid )
import Options.Applicative hiding ( action )
import Shelly              hiding ( FilePath )
import Prelude             hiding ( readFile, words, writeFile )

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
  { aDomain    :: Domain
  , aConfig    :: FilePath
  , aQueue     :: Queue
  , aContainer :: FilePath
  , aPail      :: Pail
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
          <> help    "AWS SWF Service domain" )
      <*> strOption
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
          <> help    "AWS SWF Service Flow worker container" )
      <*> strOption
          (  long    "bucket"
          <> short   'b'
          <> metavar "NAME"
          <> help    "AWS S3 Service bucket" ) where
        args domain config queue container pail = Args
          { aDomain    = pack domain
          , aConfig    = config
          , aQueue     = pack queue
          , aContainer = container
          , aPail      = pack pail
          }

exec :: MonadIO m => Container -> Pail -> Metadata -> m Metadata
exec container pail metadata =
  shelly $ withDir $ \dataDir storeDir -> do
    input dataDir metadata
    docker dataDir container
    store storeDir
    output dataDir where
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
      docker dir Container{..} =
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
                append (toTextIgnore dir) ":/app/data" : cVolumes
      store dir = do
        return ()
        -- artifacts <- findWhen test_f dir
        -- forM_ artifacts $ \artifact -> do
        --   key <- relativeTo dir artifact
        --   putter pail (toTextIgnore key) (unpack $ toTextIgnore artifact)

main :: IO ()
main =
  execParser argsPI >>= call where
    call Args{..} = do
      config <- decodeFile aConfig >>= hoistMaybe "Bad Config"
      container <- decodeFile aContainer >>= hoistMaybe "Bad Container"
      env <- flowEnv config
      forever $ do
        uid <- newUid
        r <- runFlowT env $
          act aDomain uid aQueue (exec container aPail)
        print r where
          hoistMaybe s =
            maybe (error s) return
