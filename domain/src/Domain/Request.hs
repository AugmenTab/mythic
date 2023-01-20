module Domain.Request
  ( SheetSubject, sheetSubjectText
  , sheetDataMap
  , makeSheetRequest
  , setSheetQueryStrings
  ) where

import           Flipstone.Prelude

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

data SheetSubject
  = ArmorSheet
  | EquipmentSheet
  | MeleeWeaponSheet
  | RangedWeaponSheet
  deriving stock (Eq, Ord)

sheetSubjectText :: SheetSubject -> Text
sheetSubjectText subject =
  case subject of
    ArmorSheet        -> "ArmorSheet"
    EquipmentSheet    -> "EquipmentSheet"
    MeleeWeaponSheet  -> "MeleeWeaponSheet"
    RangedWeaponSheet -> "RangedWeaponSheet"

newtype GID = GID BS.ByteString
newtype Range = Range BS.ByteString

type SheetData = (GID, Range)

sheetDataMap :: Map.Map SheetSubject SheetData
sheetDataMap =
  Map.fromList
    [ ( ArmorSheet       , (GID "3822484"  , Range "B1:M295") )
    , ( EquipmentSheet   , (GID "515202982", Range "B1:E346") )
    , ( MeleeWeaponSheet , (GID "346860164", Range "B1:Q51")  )
    , ( RangedWeaponSheet, (GID "297713635", Range "B1:P375") )
    ]

makeSheetRequest :: Either Text HTTP.Request
makeSheetRequest =
  let baseURL = "https://docs.google.com"
      path = "/spreadsheets/d/1oebS4iy37YYpuooGVI_7WpSOCk2Fy_zh2ACC6_heLJA/export"
      buildRequest =
          Right
        . HTTP.addRequestHeader "Accept" "text/csv"
        . HTTP.setRequestPath path
        . HTTP.setRequestSecure True

   in maybe (Left "Couldn't parse URL") buildRequest $ HTTP.parseRequest baseURL

setSheetQueryStrings :: SheetData -> HTTP.Request -> HTTP.Request
setSheetQueryStrings (GID gid, Range range) =
  HTTP.setQueryString
    [ ("format", Just "csv")
    , ("gid", Just gid)
    , ("range", Just range)
    ]
