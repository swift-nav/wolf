{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Files, directories, and encoding / decoding file functions.
--
module Network.AWS.Wolf.File
  ( findRegularFiles
  , touchDirectory
  , dataDirectory
  , storeDirectory
  , inputDirectory
  , outputDirectory
  , writeText
  , readText
  , writeJson
  , readYaml
  , withCurrentWorkDirectory
  ) where

import           Data.Aeson
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Time
import           Data.Yaml                hiding (encode)
import           Network.AWS.Wolf.Prelude hiding (find)
import           System.Directory
import           System.FilePath
import           System.FilePath.Find
import           System.IO                hiding (readFile, writeFile)

-- | Recursively find all files under a directory.
--
findRegularFiles :: MonadIO m => FilePath -> m [FilePath]
findRegularFiles =
  liftIO . find always (fileType ==? RegularFile)

-- | Create parent directory of file if missing.
--
touchDirectory :: MonadIO m => FilePath -> m ()
touchDirectory =
  liftIO . createDirectoryIfMissing True . takeDirectory

-- | Determine path to data directory and create it.
--
dataDirectory :: MonadIO m => FilePath -> m FilePath
dataDirectory dir = do
  let dir' = dir </> "data"
  liftIO $ createDirectoryIfMissing True dir'
  pure dir'

-- | Determine path to store directory and create it.
--
storeDirectory :: MonadIO m => FilePath -> m FilePath
storeDirectory dir = do
  let dir' = dir </> "store"
  liftIO $ createDirectoryIfMissing True dir'
  pure dir'

-- | Determine path to store input directory and create it.
--
inputDirectory :: MonadIO m => FilePath -> m FilePath
inputDirectory dir = do
  let dir' = dir </> "input"
  liftIO $ createDirectoryIfMissing True dir'
  pure dir'

-- | Determine path to store output directory and create it.
--
outputDirectory :: MonadIO m => FilePath -> m FilePath
outputDirectory dir = do
  let dir' = dir </> "output"
  liftIO $ createDirectoryIfMissing True dir'
  pure dir'

-- | Maybe write text to a file.
--
writeText :: MonadIO m => FilePath -> Maybe Text -> m ()
writeText file contents =
  liftIO $ void $ traverse (writeFile file) contents

-- | Maybe read text from a file.
--
readText :: MonadIO m => FilePath -> m (Maybe Text)
readText file =
 liftIO $ do
   b <- doesFileExist file
   if not b then pure mempty else
     pure <$> readFile file

-- | Encode from JSON and write file.
--
writeJson :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeJson file item =
  liftIO $ withFile file WriteMode $ \h ->
    BS.hPut h $ LBS.toStrict $ encode item

-- | Read file and decode it from YAML.
--
readYaml :: (MonadIO m, FromJSON a) => FilePath -> m a
readYaml file =
  liftIO $ withFile file ReadMode $ \h -> do
    body <- BS.hGetContents h
    eitherThrowIO' $ decodeEither body

-- | Get a temporary timestamped work directory.
--
getWorkDirectory :: MonadIO m => Text -> Bool -> m FilePath
getWorkDirectory uid local =
  liftIO $ do
    td   <- bool getTemporaryDirectory getCurrentDirectory local
    time <- getCurrentTime
    let dir = td </> formatTime defaultTimeLocale "%FT%T%z" time </> textToString uid
    createDirectoryIfMissing True dir
    pure dir

-- | Copy directory contents recursively.
--
copyDirectoryRecursive :: MonadIO m => FilePath -> FilePath -> m ()
copyDirectoryRecursive fd td =
  liftIO $ do
    createDirectoryIfMissing True td
    cs <- filter (`notElem` [".", ".."]) <$> getDirectoryContents fd
    forM_ cs $ \c -> do
      let fc = fd </> c
          tc = td </> c
      e <- doesDirectoryExist fc
      bool (copyFile fc tc) (copyDirectoryRecursive fc tc) e

-- | Setup a temporary work directory.
--
withWorkDirectory :: MonadControl m => Text -> Bool -> (FilePath -> m a) -> m a
withWorkDirectory uid local =
  bracket (getWorkDirectory uid local) (liftIO . removeDirectoryRecursive)

-- | Change to directory and then return to current directory.
--
withCurrentDirectory' :: MonadControl m => FilePath -> (FilePath -> m a) -> m a
withCurrentDirectory' wd action =
  bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \cd -> do
    liftIO $ setCurrentDirectory wd
    action cd

-- | Setup a temporary work directory and copy current directory files to it.
--
withCurrentWorkDirectory :: MonadControl m => Text -> Bool -> Bool -> (FilePath -> m a) -> m a
withCurrentWorkDirectory uid nocopy local action =
  withWorkDirectory uid local $ \wd ->
    withCurrentDirectory' wd $ \cd -> do
      unless (nocopy || local) $
        copyDirectoryRecursive cd wd
      action wd

