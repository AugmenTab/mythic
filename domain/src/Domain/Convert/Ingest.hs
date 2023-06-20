module Domain.Convert.Ingest
  ( ingestRaw
  ) where

import           Flipstone.Prelude
import           Domain.CSV
import qualified Domain.Request as Request
import           Data.Types

import qualified Data.ByteString.Lazy as LBS
import           Data.Either.Extra (eitherToMaybe)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

ingestRaw :: Request.SheetSubject
          -> Request.SheetLines
          -> Either T.Text (CompendiumMap [RawData])
ingestRaw subject lines = do
  csv <-
    if L.null lines
       then Left $ "No content in " <> Request.sheetSubjectText subject <> "."
       else pure $ T.unlines $ L.filter (not . isEmptyLine) lines

  let ingestFn =
        case subject of
          Request.AbilitySheet      -> ingestAbility
          Request.ArmorSheet        -> ingestArmor
          Request.EquipmentSheet    -> ingestEquipment
          Request.MeleeWeaponSheet  -> ingestMelee
          Request.RangedWeaponSheet -> ingestRanged

  fmap (Map.fromListWith (<>))
    $ traverse (mkCompendiumMapEntry subject) =<< ingestFn csv

isEmptyLine :: T.Text -> Bool
isEmptyLine line =
  L.any (flip T.isPrefixOf line) [ ",", "Default," ]

mkCompendiumMapEntry :: Request.SheetSubject
                     -> RawData
                     -> Either T.Text (CompendiumData, [RawData])
mkCompendiumMapEntry subject rawData = do
  let faction =
        eitherToMaybe
          . factionFromText
          $ case rawData of
              AbilityData   _   -> "factionless_ability"
              ArmorData     raw -> rawArmorFaction     raw
              EquipmentData raw -> rawEquipmentFaction raw
              MeleeData     raw -> rawMeleeFaction     raw
              RangedData    raw -> rawRangedFaction    raw

  pure ( (faction, mkCompendiumDetails $ Request.sheetSubjectTitle subject)
       , [ rawData ]
       )

ingestAbility :: T.Text -> Either T.Text [RawData]
ingestAbility =
  ffmap AbilityData . decodeCSV . LBS.fromStrict . TE.encodeUtf8

ingestArmor :: T.Text -> Either T.Text [RawData]
ingestArmor _ =
  -- ffmap ArmorData . decodeCSV . LBS.fromStrict . TE.encodeUtf8
  Left "Not implemented yet"

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
