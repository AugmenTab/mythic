module Domain.Request
  ( SheetSubject(..)
  , sheetSubjectText
  , sheetSubjectTitle

  , SheetHeader(..)
  , SheetLines

  , sheetDataMap
  , makeSheetRequest
  , setSheetQueryStrings
  , responseContent
  ) where

import           Flipstone.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

data SheetSubject
  = AbilitySheet
  | ArmorSheet
  | EquipmentSheet
  | MeleeWeaponSheet
  | RangedWeaponSheet
  deriving stock (Eq, Ord)

sheetSubjectText :: SheetSubject -> T.Text
sheetSubjectText subject =
  case subject of
    AbilitySheet      -> "AbilitySheet"
    ArmorSheet        -> "ArmorSheet"
    EquipmentSheet    -> "EquipmentSheet"
    MeleeWeaponSheet  -> "MeleeWeaponSheet"
    RangedWeaponSheet -> "RangedWeaponSheet"

sheetSubjectTitle :: SheetSubject -> T.Text
sheetSubjectTitle subject =
  case subject of
    AbilitySheet      -> "Abilities"
    ArmorSheet        -> "Armor"
    EquipmentSheet    -> "Equipment"
    MeleeWeaponSheet  -> "Melee Weapons"
    RangedWeaponSheet -> "Ranged Weapons"

newtype GID = GID BS.ByteString
newtype Range = Range BS.ByteString

type SheetData = (GID, Range)

sheetDataMap :: Map.Map SheetSubject SheetData
sheetDataMap =
  Map.fromList
    [ ( AbilitySheet     , (GID "1007822165", Range "A2:F96")  )
 -- , ( ArmorSheet       , (GID "3822484"   , Range "A2:M170") )
    , ( EquipmentSheet   , (GID "515202982" , Range "A2:F607") )
 -- , ( MeleeWeaponSheet , (GID "346860164" , Range "A2:R49")  )
 -- , ( RangedWeaponSheet, (GID "297713635" , Range "A2:Q331") )
    ]

makeSheetRequest :: Either T.Text HTTP.Request
makeSheetRequest =
  let baseURL = "https://docs.google.com"
      buildRequest =
        Right
          . HTTP.addRequestHeader "Accept" "text/csv"
          . HTTP.setRequestPath path
          . HTTP.setRequestSecure True

   in maybe (Left "Couldn't parse URL") buildRequest $ HTTP.parseRequest baseURL

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

-- This newtype wrapper prevents us from accidentally including the sheet header
-- in the lines that will be converted to Items.
newtype SheetHeader = SheetHeader { unSheetHeader :: T.Text }

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
