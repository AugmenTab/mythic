module Domain.Convert.Ingest
  ( ingestRaw
  ) where

import           Flipstone.Prelude
import           Domain.CSV
import qualified Domain.Request as Request
import           Data.Types

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

ingestRaw :: Request.SheetSubject
          -> CompendiumMap T.Text
          -> Either T.Text (CompendiumMap [RawData])
ingestRaw subject cMap = do
  case subject of
    Request.ArmorSheet        -> Left "Not implemented yet" -- TODO: traverse ingestArmor     cMap
    Request.EquipmentSheet    -> traverse ingestEquipment cMap
    Request.MeleeWeaponSheet  -> traverse ingestMelee     cMap
    Request.RangedWeaponSheet -> traverse ingestRanged    cMap

ingestEquipment :: T.Text -> Either T.Text [RawData]
ingestEquipment =
  ffmap EquipmentData . decodeCSV . LBS.fromStrict . TE.encodeUtf8

ingestMelee :: T.Text -> Either T.Text [RawData]
ingestMelee _ =
--ffmap MeleeData . decodeCSV . LBS.fromStrict . TE.encodeUtf8
  Left "Not implemented yet"

ingestRanged :: T.Text -> Either T.Text [RawData]
ingestRanged =
  ffmap RangedData . decodeCSV . LBS.fromStrict . TE.encodeUtf8
