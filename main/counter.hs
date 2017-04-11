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
  { config :: FilePath
    -- ^ Configuration file.
  , plan   :: FilePath
    -- ^ Plan file to count on.
  } deriving (Show, Generic)

instance ParseRecord Args

-- | Run counter.
--
main :: IO ()
main = do
  args <- getRecord "Counter"
  countMain
    (config args)
    (plan args)
