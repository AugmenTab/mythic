module Domain.Convert.Ingest
  ( ingestRaw
  ) where

import           Flipstone.Prelude
import           Domain.CSV
import qualified Domain.Request as Request
import           Data.Types

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import           Data.Traversable (sequence)

ingestRaw :: Request.SheetSubject
          -> [CompendiumData Text]
          -> Either Text [CompendiumData RawData]
ingestRaw subject sheets =
  let ingestSheet =
        case subject of
       -- Request.ArmorSheet        -> ingestArmor -- TODO
          Request.EquipmentSheet    -> ingestEquipment
       -- Request.MeleeWeaponSheet  -> ingestMelee -- TODO
       -- Request.RangedWeaponSheet -> ingestRanged -- TODO

   in fmap concat $ sequence $ ingestSheet <$> sheets

ingestEquipment :: CompendiumData Text
                -> Either Text [CompendiumData RawData]
ingestEquipment (CompendiumData (faction, content, sheet)) = do
  rawEquipment <- decodeCSV . LBS.fromStrict $ TE.encodeUtf8 sheet
  pure $ (\e -> CompendiumData (faction, content, EquipmentData e)) <$> rawEquipment
