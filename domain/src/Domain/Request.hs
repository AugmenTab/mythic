module Domain.Request
  ( SheetSubject(..)
  , sheetSubjectText
  , sheetSubjectTitle

  , SheetLines

  , sheetDataMap
  , makeSheetRequest
  , setSheetQueryStrings
  , responseContent
  ) where

import           Flipstone.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Either.Extra (maybeToEither)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

data SheetSubject
  = AbilitySheet
  | ArmorSheet
  | BestiarySheet
  | EquipmentSheet
  | FloodSheet
  | MeleeWeaponSheet
  | PermutationSheet
  | RangedWeaponSheet
  | VehicleSheet
  deriving stock (Eq, Ord)

sheetSubjectText :: SheetSubject -> T.Text
sheetSubjectText subject =
  case subject of
    AbilitySheet      -> "AbilitySheet"
    ArmorSheet        -> "ArmorSheet"
    BestiarySheet     -> "BestiarySheet"
    EquipmentSheet    -> "EquipmentSheet"
    FloodSheet        -> "FloodSheet"
    MeleeWeaponSheet  -> "MeleeWeaponSheet"
    PermutationSheet  -> "PermutationSheet"
    RangedWeaponSheet -> "RangedWeaponSheet"
    VehicleSheet      -> "VehicleSheet"

sheetSubjectTitle :: SheetSubject -> T.Text
sheetSubjectTitle subject =
  case subject of
    AbilitySheet      -> "Abilities"
    ArmorSheet        -> "Armor"
    BestiarySheet     -> "Bestiary"
    EquipmentSheet    -> "Equipment"
    FloodSheet        -> "Bestiary - The Flood"
    MeleeWeaponSheet  -> "Melee Weapons"
    PermutationSheet  -> "Armor Permutations"
    RangedWeaponSheet -> "Ranged Weapons"
    VehicleSheet      -> "Vehicles"

newtype GID = GID BS.ByteString
newtype Range = Range BS.ByteString

type SheetData = (GID, Range)

sheetDataMap :: Map.Map SheetSubject SheetData
sheetDataMap =
  Map.fromList
    [ ( AbilitySheet     , (GID "1007822165", Range "A2:F96")   )
    , ( ArmorSheet       , (GID "226189720" , Range "A2:P199")  )
    , ( BestiarySheet    , (GID "1982557897", Range "A2:BX407") )
    , ( EquipmentSheet   , (GID "1641182149", Range "A2:F580")  )
    , ( FloodSheet       , (GID "1809814064", Range "A2:AA381") )
    , ( MeleeWeaponSheet , (GID "346860164" , Range "A2:AV63")  )
    , ( PermutationSheet , (GID "80923077"  , Range "A2:G84")   )
    , ( RangedWeaponSheet, (GID "1510373161", Range "B2:AS397") )
    , ( VehicleSheet     , (GID "144762228" , Range "A2:KU475") )
    ]

makeSheetRequest :: Either T.Text HTTP.Request
makeSheetRequest =
  let baseURL = "https://docs.google.com"
      buildRequest =
        HTTP.setRequestResponseTimeout sheetTimeout
          . HTTP.addRequestHeader "Accept" "text/csv"
          . HTTP.setRequestPath path
          . HTTP.setRequestSecure True

   in maybeToEither "Couldn't parse URL"
        . fmap buildRequest
        $ HTTP.parseRequest baseURL

sheetTimeout :: HTTP.ResponseTimeout
sheetTimeout = HTTP.responseTimeoutMicro $ 2 * 60 * 1000000 -- 2 minutes

path :: BS.ByteString
path =
  BS.intercalate "/"
    [ "/spreadsheets"
    , "d"
    , "1oebS4iy37YYpuooGVI_7WpSOCk2Fy_zh2ACC6_heLJA"
    , "export"
    ]

setSheetQueryStrings :: SheetData -> HTTP.Request -> HTTP.Request
setSheetQueryStrings (GID gid, Range range) =
  HTTP.setQueryString
    [ ("format", Just "csv")
    , ("gid"   , Just gid)
    , ("range" , Just range)
    ]

-- This type alias separates the common T.Text namespace from the intent behind
-- these CSV rows - to eventually be converted into Items.
type SheetLines = [T.Text]

responseContent :: HTTP.Response LBS.ByteString
                -> SheetSubject
                -> Either T.Text SheetLines
responseContent resp subject =
  case TE.decodeUtf8 . LBS.toStrict $ HTTP.responseBody resp of
    txt | T.null txt -> Left  $ "No content in " <> sheetSubjectText subject
        | otherwise  -> Right $ T.lines txt
