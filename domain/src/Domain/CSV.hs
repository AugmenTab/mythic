module Domain.CSV
  ( decodeCSV
  ) where

import           Flipstone.Prelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as CSV
import qualified Data.Text as T
import           Data.Tuple (snd)
import qualified Data.Vector as V

decodeCSV :: CSV.FromNamedRecord a => LBS.ByteString -> Either Text [a]
decodeCSV bs =
  case CSV.decodeByName bs of
    Left  err -> Left $ T.pack err
    Right raw -> Right . V.toList $ snd raw
