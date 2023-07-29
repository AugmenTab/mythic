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
  , nullJSON
  , valueInt
  , valueRatio
  , valueText
  ) where

import           Flipstone.Prelude

import           Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.Aeson.Encode.Pretty as Pretty
import           Data.Aeson.Key (Key, fromText)
import           Data.Aeson.Types (Value(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           GHC.Types (Double)

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

encodeLine :: ToJSON a => a -> BS.ByteString
encodeLine =
  TE.encodeUtf8
    . encodeJSON (defaultConfig { Pretty.confIndent = Pretty.Spaces 0 })

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

nullJSON :: Value
nullJSON = Null

valueInt :: Int -> Value
valueInt = toJSON

valueRatio :: Rational -> Value
valueRatio = (toJSON :: Double -> Value) . realToFrac

valueText :: T.Text -> Value
valueText = toJSON
