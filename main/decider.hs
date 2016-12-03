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
    -- ^ Plan file to decide on.
  } deriving (Show, Generic)

instance ParseRecord Args

-- | Run decider.
--
main :: IO ()
main = do
  args <- getRecord "Decider"
  runResourceT $ decideMain
    (config args)
    (plan args)
