{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run upload.
--
import Network.AWS.Wolf
import Options.Generic

-- | Args
--
-- Program arguments.
--
data Args = Args
  { config :: FilePath
    -- ^ Configuration file.
  , dir    :: FilePath
    -- ^ Directory to download to.
  , uid    :: Text
    -- ^ Workflow ID to download.
  } deriving (Show, Generic)

instance ParseRecord Args

-- | Run counter.
--
main :: IO ()
main = do
  args <- getRecord "Download"
  downMain
    (config args)
    (dir args)
    (uid args)
