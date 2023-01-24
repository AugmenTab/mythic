module Domain.Convert.Ingest
  ( ingestRaw
  ) where

import           Flipstone.Prelude
import           Domain.CSV
import qualified Domain.Request as Request
import           Data.Types

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE

ingestRaw :: Request.SheetSubject
          -> CompendiumMap Text
          -> Either Text (CompendiumMap [RawData])
ingestRaw subject cMap = do
  case subject of
 -- Request.ArmorSheet        -> traverse ingestArmor     cMap -- TODO
    Request.EquipmentSheet    -> traverse ingestEquipment cMap
 -- Request.MeleeWeaponSheet  -> traverse ingestMelee     cMap -- TODO
 -- Request.RangedWeaponSheet -> traverse ingestRanged    cMap -- TODO

ingestEquipment :: Text -> Either Text [RawData]
ingestEquipment sheet = do
  rawEquipment <- decodeCSV . LBS.fromStrict $ TE.encodeUtf8 sheet
  pure $ EquipmentData <$> rawEquipment
