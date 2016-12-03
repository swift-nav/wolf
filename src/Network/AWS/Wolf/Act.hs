{-# LANGUAGE NoImplicitPrelude #-}

-- | SWF Actor logic.
--
module Network.AWS.Wolf.Act
  ( act
  , actMain
  ) where

import Data.Aeson
import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.File
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.S3
import Network.AWS.Wolf.SWF
import Network.AWS.Wolf.Trace
import Network.AWS.Wolf.Types
import System.Process

download :: MonadAmazonStore c m => FilePath -> m ()
download dir = do
  ks <- listArtifacts
  forM_ ks $ \k -> do
    traceInfo "get-artifact" [ "key" .= k ]
    getArtifact (dir </> textToString k) k

upload :: MonadAmazonStore c m => FilePath -> m ()
upload dir = do
  fs <- findRegularFiles dir
  forM_ fs $ \f -> do
    let k = stripPrefix' (textFromString dir) (textFromString f)
    traceInfo "put-artifact" [ "key" .= k ]
    traverse (putArtifact f) k

callCommand' :: MonadMain m => String -> m (Maybe SomeException)
callCommand' command =
  handle (return . Just) $ do
    liftIO $ callCommand command
    return Nothing

run :: MonadCtx c m => String -> m (Maybe SomeException)
run command =
  preCtx [ "command" .= command ] $ do
    traceInfo "begin" mempty
    e <- callCommand' command
    traceInfo "end" [ "exception" .= (displayException <$> e) ]
    return e

act :: MonadConf c m => Text -> String -> m ()
act queue command =
  preConfCtx [ "label" .= LabelAct ] $
    runAmazonCtx $
      runAmazonWorkCtx queue $ do
        traceInfo "poll" mempty
        (token, uid, input) <- pollActivity
        maybe_ token $ \token' ->
          maybe_ uid $ \uid' ->
            withCurrentWorkDirectory uid' $ \wd ->
              runAmazonStoreCtx uid' $ do
                traceInfo "start" [ "input" .= input, "dir" .= wd ]
                dd  <- dataDirectory wd
                sd  <- storeDirectory wd
                isd <- inputDirectory sd
                osd <- outputDirectory sd
                writeJson (dd </> "control.json") (Control uid')
                writeText (dd </> "input.json") input
                download isd
                e <- run command
                upload osd
                output <- readText (dd </> "output.json")
                maybe (completeActivity token' output) (const $ failActivity token') e
                traceInfo "finish" [ "output" .= output ]

actMain :: MonadMain m => FilePath -> Text -> String -> m ()
actMain cf queue command =
  runCtx $ do
    conf <- readYaml cf
    runConfCtx conf $
      forever $ act queue command
