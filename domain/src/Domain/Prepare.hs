module Domain.Prepare
  ( prepareSheet
  ) where

import           Flipstone.Prelude
import qualified Domain.Request as Request
import           Data.Types

import qualified Data.Map as Map
import qualified Data.Text as T

prepareSheet :: Request.SheetSubject
             -> [T.Text]
             -> Either T.Text (CompendiumMap T.Text)
prepareSheet subject [] =
  Left $ "No content in " <> Request.sheetSubjectText subject

prepareSheet subject (header:lines) =
  let (separateSheet, startingContent) =
        case subject of
      --  Request.ArmorSheet        -> startingArmor -- TODO
          Request.EquipmentSheet    -> (separateEquipment, startingEquipment)
      --  Request.MeleeWeaponSheet  -> startingMelee -- TODO
      --  Request.RangedWeaponSheet -> startingRanged -- TODO

   in Right $ separateSheet header UNSC startingContent [] Map.empty lines

startingEquipment :: CompendiumDetails
startingEquipment = mkCompendiumDetails "HELMET AND FACIAL EQUIPMENT"

separateEquipment :: T.Text
                  -> Faction
                  -> CompendiumDetails
                  -> [T.Text]
                  -> CompendiumMap T.Text
                  -> [T.Text]
                  -> CompendiumMap T.Text
separateEquipment header faction content sheet cMap [] =
  Map.insert (faction, content) (T.unlines $ header : sheet) cMap

separateEquipment header faction content sheet cMap (l1:[]) =
  Map.insert (faction, content) (T.unlines $ header : l1 : sheet) cMap

separateEquipment header faction content sheet cMap (l1:l2:lines) =
  case (tryParseFactionOrContent l1, tryParseFactionOrContent l2) of
    (Just (Left f), Just (Right c)) ->
      separateEquipment
        header
        f
        c
        []
        (Map.insert (faction, content) (T.unlines $ header : sheet) cMap)
        lines

    (Just (Right c), _) ->
      separateEquipment
        header
        faction
        c
        []
        (Map.insert (faction, content) (T.unlines $ header : sheet) cMap)
        (l2 : lines)

    _ ->
      separateEquipment header faction content (l1 : sheet) cMap (l2 : lines)

tryParseFactionOrContent :: T.Text -> Maybe (Either Faction CompendiumDetails)
tryParseFactionOrContent line
  | T.isPrefixOf ",Equipment,"       line
  , T.isInfixOf  "COVENANT,COVENANT" line = Just $ Left Covenant

  -- This is ugly. If we reach this point, we're on a compendium title row. The
  -- only consistent way I can find to parse these out is to check for the
  -- presence of two commas somewhere in the line, which would be the commas at
  -- the end of the description and weight columns respectively. This should
  -- only hit rows that are missing both a price and a weight figure, so this
  -- should only occur with compendium title rows.
  --
  -- Then, we're stripping the first 11 characters off the string, which would
  -- be equal to ",Equipment,". Since we have already ruled out the row being a
  -- faction separator row, this SHOULD get us at the start of the compendium
  -- title row. None of those have a comma, so we should be able to safely go
  -- up to the next column and use that collected text.
  | T.isPrefixOf ",Equipment," line
  , T.isInfixOf  ",,"          line =
    Just . Right
         . mkCompendiumDetails
         . T.takeWhile (not . (==) ',')
         $ T.drop 11 line

  | otherwise = Nothing
