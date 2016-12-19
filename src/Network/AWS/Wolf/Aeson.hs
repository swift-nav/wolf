{-# LANGUAGE NoImplicitPrelude #-}

-- | Aeson generic deriving options.
--
module Network.AWS.Wolf.Aeson
  ( camelOptions
  , snakeOptions
  , spinalOptions
  ) where

import Data.Aeson.Types
import Data.Char
import Data.Text
import Data.Text.Manipulate
import Network.AWS.Wolf.Prelude hiding (dropWhile)

-- | Remove characters up to the first upper case character.
--
unprefix :: Text -> Text
unprefix = dropWhile (not . isUpper)

-- | Derive fields with camelCase.
--
camelOptions :: Options
camelOptions = defaultOptions
  { fieldLabelModifier     = unpack . toCamel . unprefix . pack
  , constructorTagModifier = unpack . toCamel . unprefix . lowerHead . pack
  , omitNothingFields      = True
  }

-- | Derive fields with snake_case.
--
snakeOptions :: Options
snakeOptions = defaultOptions
  { fieldLabelModifier     = unpack . toSnake . unprefix . pack
  , constructorTagModifier = unpack . toSnake . unprefix . lowerHead . pack
  , omitNothingFields      = True
  }

-- | Derive fields with spinal-case.
--
spinalOptions :: Options
spinalOptions = defaultOptions
  { fieldLabelModifier     = unpack . toSpinal . unprefix . pack
  , constructorTagModifier = unpack . toSpinal . unprefix . lowerHead . pack
  , omitNothingFields      = True
  }
