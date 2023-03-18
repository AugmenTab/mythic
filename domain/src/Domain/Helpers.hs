module Domain.Helpers
  ( tryParseInt
  ) where

import           Flipstone.Prelude

import qualified Data.Char as C
import qualified Data.Text as T
import           Text.Read (readMaybe)

tryParseInt :: T.Text -> Maybe Int
tryParseInt = readMaybe . T.unpack . T.filter C.isDigit
