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
  , queue   :: Text
    -- ^ Queue to listen to act on.
  , num     :: Maybe Int
    -- ^ Number of actors to run concurrently.
  , nocopy  :: Bool
    -- ^ Copy working directory.
  , local   :: Bool
    -- ^ Run locally, not in a temp directory.
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
    (queue args)
    (fromMaybe 1 $ num args)
    (nocopy args)
    (local args)
    (command args)
