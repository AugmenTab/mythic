module Domain.Request
  ( SheetSubject(..)
  , sheetSubjectText
  , sheetDataMap
  , makeSheetRequest
  , setSheetQueryStrings
  , responseContent
  ) where

import           Flipstone.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

data SheetSubject
  = ArmorSheet
  | EquipmentSheet
  | MeleeWeaponSheet
  | RangedWeaponSheet
  deriving stock (Eq, Ord)

sheetSubjectText :: SheetSubject -> T.Text
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
 -- [ ( ArmorSheet       , (GID "3822484"  , Range "A2:M295") )
    [ ( EquipmentSheet   , (GID "515202982", Range "A2:E234") ) -- TODO: Figure out weapon mods
 -- , ( MeleeWeaponSheet , (GID "346860164", Range "A2:Q51")  )
 -- , ( RangedWeaponSheet, (GID "297713635", Range "A2:P375") )
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

responseContent :: HTTP.Response LBS.ByteString
                -> SheetSubject
                -> Either T.Text [T.Text]
responseContent resp subject =
  case TE.decodeUtf8 . LBS.toStrict $ HTTP.responseBody resp of
    txt | T.null txt -> Left  $ "No content in " <> sheetSubjectText subject
        | otherwise  -> Right $ T.lines txt
