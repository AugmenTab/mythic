module Domain.JSON
  ( ToJSON(..)
  , Value(..)
  , object
  , withObject
  , (.=)

  , emptyArray
  , encodeJSON
  , emptyObject
  , keyFromText
  , valueInt
  , valueText
  ) where

import           Flipstone.Prelude

import           Data.Aeson (ToJSON(..), encode, withObject, object, (.=))
import           Data.Aeson.Key (Key, fromText)
import           Data.Aeson.Types (Value(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

emptyArray :: Value
emptyArray = Array V.empty

encodeJSON :: ToJSON a => a -> Text
encodeJSON = TE.decodeUtf8 . LBS.toStrict . encode

emptyObject :: Value
emptyObject = object []

keyFromText :: Text -> Key
keyFromText = fromText

valueInt :: Int -> Value
valueInt = toJSON

valueText :: Text -> Value
valueText = toJSON
