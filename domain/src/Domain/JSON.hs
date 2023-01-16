module Domain.JSON
  ( ToJSON(..)
  , Value(..)
  , object
  , withObject
  , (.=)

  , defaultInt
  , emptyArray
  , encodeJSON
  , emptyObject
  ) where

import           Flipstone.Prelude

import           Data.Aeson (ToJSON(..), encode, withObject, object, (.=))
import           Data.Aeson.Types (Value(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

defaultInt :: Int -> Value
defaultInt = toJSON

emptyArray :: Value
emptyArray = Array V.empty

encodeJSON :: ToJSON a => a -> Text
encodeJSON = TE.decodeUtf8 . LBS.toStrict . encode

emptyObject :: Value
emptyObject = object []
