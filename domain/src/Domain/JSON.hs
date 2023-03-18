module Domain.JSON
  ( ToJSON(..)
  , Value(..)
  , object
  , (.=)

  , encodeLine
  , encodePage

  , emptyArray
  , emptyObject
  , keyFromText
  , valueInt
  , valueText
  ) where

import           Flipstone.Prelude

import           Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.Aeson.Encode.Pretty as Pretty
import           Data.Aeson.Key (Key, fromText)
import           Data.Aeson.Types (Value(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

defaultConfig :: Pretty.Config
defaultConfig =
  Pretty.Config
    { Pretty.confIndent          = Pretty.Spaces 2
    , Pretty.confCompare         = const . const LT
    , Pretty.confNumFormat       = Pretty.Generic
    , Pretty.confTrailingNewline = False
    }

--
-- Encoding JSON
--
encodeJSON :: ToJSON a => Pretty.Config -> a -> T.Text
encodeJSON config = TE.decodeUtf8 . LBS.toStrict . Pretty.encodePretty' config

encodeLine :: ToJSON a => a -> T.Text
encodeLine = encodeJSON (defaultConfig { Pretty.confIndent = Pretty.Spaces 0 })

encodePage :: ToJSON a => a -> T.Text
encodePage = encodeJSON defaultConfig

--
-- ToJSON Implementation Helpers
--
emptyArray :: Value
emptyArray = Array V.empty

emptyObject :: Value
emptyObject = object []

keyFromText :: T.Text -> Key
keyFromText = fromText

valueInt :: Int -> Value
valueInt = toJSON

valueText :: T.Text -> Value
valueText = toJSON
