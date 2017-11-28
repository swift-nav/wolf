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
    -- ^ Directory to upload.
  , uid    :: Text
    -- ^ Workflow ID to upload.
  } deriving (Show, Generic)

instance ParseRecord Args

-- | Run counter.
--
main :: IO ()
main = do
  args <- getRecord "Upload"
  upMain
    (config args)
    (dir args)
    (uid args)
