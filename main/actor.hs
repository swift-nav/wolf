{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run actor.
--
import Network.AWS.Wolf
import Options.Generic

-- | Args
--
-- Program arguments.
--
data Args = Args
  { config  :: FilePath
    -- ^ Configuration file.
  , storeconf :: Maybe Bool
    -- ^ Optional copy configuration file to output dorectory.
  , quiesce :: Maybe FilePath
    -- ^ Optional quiesce file to stop actor.
  , domain  :: Maybe Text
    -- ^ Optional domain to use.
  , bucket  :: Maybe Text
    -- ^ Optional bucket to use.
  , prefix  :: Maybe Text
    -- ^ Optional prefix to use.
  , queue   :: Text
    -- ^ Queue to listen to act on.
  , num     :: Maybe Int
    -- ^ Number of actors to run concurrently.
  , nocopy  :: Bool
    -- ^ Copy working directory.
  , local   :: Bool
    -- ^ Run locally, not in a temp directory.
  , include :: [FilePath]
    -- ^ Optional artifacts to filter.
  , command :: String
    -- ^ Command to run.
  } deriving (Show, Generic)

instance ParseRecord Args

-- | Run actor.
--
main :: IO ()
main = do
  args <- getRecord "Actor"
  actMain
    (config args)
    (fromMaybe False $ storeconf args)
    (quiesce args)
    (domain args)
    (bucket args)
    (prefix args)
    (queue args)
    (fromMaybe 1 $ num args)
    (nocopy args)
    (local args)
    (include args)
    (command args)
