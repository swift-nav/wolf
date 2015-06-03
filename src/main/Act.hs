{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Act ( main ) where

import Control.Exception           ( SomeException )
import Control.Monad               ( forever, mzero, liftM )
import Control.Monad.IO.Class      ( MonadIO )
import Data.Text                   ( Text, pack, append )
import Data.Yaml
import Network.AWS.SWF.Flow        ( Domain, Queue, Metadata, runFlowT, act )
import Network.AWS.SWF.Flow.Helper ( flowEnv, newUid )
import Options.Applicative  hiding ( action )
import Shelly               hiding ( FilePath )
import Prelude              hiding ( readFile, writeFile )

data Container = Container
  { cImage   :: Text
  , cCommand :: Text
  , cVolumes :: [Text]
  , cDevices :: [Text]
  } deriving ( Eq, Read, Show )

instance FromJSON Container where
  parseJSON (Object v) =
    Container              <$>
    v .: "image"           <*>
    v .: "command"         <*>
    v .:? "volumes" .!= [] <*>
    v .:? "devices" .!= []
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

exec :: MonadIO m => Container -> Metadata -> m Metadata
exec container metadata =
  shelly $ withTmpDir $ \dir -> do
    input dir metadata
    docker dir container
    output dir where
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
          , [cCommand]
          ] where
            devices =
              concatMap (("--device" :) . return) cDevices
            volumes =
              concatMap (("--volume" :) . return) $
                append (toTextIgnore dir) ":/app/data" : cVolumes

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
          act aDomain uid aQueue (exec container)
        print r where
          hoistMaybe s =
            maybe (error s) return
