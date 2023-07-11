module Domain.Convert.Ingest
  ( ingestRaw
  ) where

import           Flipstone.Prelude
import           Domain.CSV
import qualified Domain.Request as Request
import           Data.Types

import qualified Data.ByteString.Lazy as LBS
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
          Request.BestiarySheet     -> ingestBestiary
          Request.EquipmentSheet    -> ingestEquipment
          Request.FloodSheet        -> ingestFlood
          Request.MeleeWeaponSheet  -> ingestMelee
          Request.PermutationSheet  -> ingestPermutation
          Request.RangedWeaponSheet -> ingestRanged
          Request.VehicleSheet      -> ingestVehicle

  fmap (Map.fromListWith (<>))
    $ traverse (mkCompendiumMapEntry subject) =<< ingestFn csv

-- NOTE: This CANNOT exclude a double-apostrophe character by itself (\"). It
-- will fail to read in Forerunner equipment due to the use of commas in the
-- name field.
isEmptyLine :: T.Text -> Bool
isEmptyLine line =
  L.any (flip T.isPrefixOf line)
    [ ","
    , "\t"
    , " "
    , "\","
    , "Default,"
    , "#REF!,"
    , "If"
    , "Sentinel Shields"
    , "The"
    , "\"The"
    , "to"
    , "BODY"
    ]

mkCompendiumMapEntry :: Request.SheetSubject
                     -> RawData
                     -> Either T.Text (CompendiumData, [RawData])
mkCompendiumMapEntry subject rawData = do
  let factionOrOther = Just . compendiumFactionFromText
      faction =
        case rawData of
          AbilityData     _   -> Nothing
          ArmorData       raw -> factionOrOther $ rawArmorFaction raw
          BestiaryData    raw -> factionOrOther $ rawBestiaryFaction raw
          EquipmentData   raw -> factionOrOther $ rawEquipmentFaction raw
          FloodData       _   -> Nothing
          MeleeData       raw -> factionOrOther $ rawMeleeFaction raw
          PermutationData raw -> factionOrOther $ rawPermutationFaction raw
          RangedData      raw -> factionOrOther $ rawRangedFaction raw
          VehicleData     raw -> factionOrOther $ rawVehicleFaction raw

  pure ( (faction, mkCompendiumDetails $ Request.sheetSubjectTitle subject)
       , [ rawData ]
       )

ingestAbility :: T.Text -> Either T.Text [RawData]
ingestAbility =
  ffmap AbilityData . decodeCSV . LBS.fromStrict . TE.encodeUtf8

ingestArmor :: T.Text -> Either T.Text [RawData]
ingestArmor = ffmap ArmorData . decodeCSV . LBS.fromStrict . TE.encodeUtf8

ingestBestiary :: T.Text -> Either T.Text [RawData]
ingestBestiary =
  ffmap BestiaryData . decodeCSV . LBS.fromStrict . TE.encodeUtf8

ingestEquipment :: T.Text -> Either T.Text [RawData]
ingestEquipment =
  ffmap EquipmentData . decodeCSV . LBS.fromStrict . TE.encodeUtf8

ingestFlood :: T.Text -> Either T.Text [RawData]
ingestFlood = ffmap FloodData . decodeCSV . LBS.fromStrict . TE.encodeUtf8

ingestMelee :: T.Text -> Either T.Text [RawData]
ingestMelee = ffmap MeleeData . decodeCSV . LBS.fromStrict . TE.encodeUtf8

ingestPermutation :: T.Text -> Either T.Text [RawData]
ingestPermutation =
  ffmap PermutationData . decodeCSV . LBS.fromStrict . TE.encodeUtf8

ingestRanged :: T.Text -> Either T.Text [RawData]
ingestRanged = ffmap RangedData . decodeCSV . LBS.fromStrict . TE.encodeUtf8

ingestVehicle :: T.Text -> Either T.Text [RawData]
ingestVehicle = ffmap VehicleData . decodeCSV . LBS.fromStrict . TE.encodeUtf8
