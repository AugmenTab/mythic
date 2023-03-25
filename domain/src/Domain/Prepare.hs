module Domain.Prepare
  ( prepareSheet
  ) where

import           Flipstone.Prelude
import qualified Domain.Request as Request
import           Data.Types

import           Control.Applicative ((<|>))
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
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
tryParseFactionOrContent line =
  case T.split (== ',') line of
    -- This guard checks for the following:
    --   - The first column is an empty string
    --   - The second column is a valid COMP_field value
    --
    -- This typically means that this line is indicating a change in either the
    -- faction or the compendium title for the upcoming items. From here, we
    -- dispatch to functions that get those values if they exist. If they don't
    -- exist, the line is likely a hidden empty line that can be safely passed
    -- over.
    --
    -- We separate out col4 from the rest of the columns because, sometimes,
    -- faction change lines will list the faction twice, and that second faction
    -- will be in the 4th column. It won't be null, but everything after it will
    -- be if the line is really a faction or compendium title change line.
    "" : col2 : col3 : _col4 : rest
      | L.all isEmptyCell rest
      , Set.member col2 fields ->
        tryParseFaction col3 <|> tryParseContent col3

    _ ->
      Nothing

-- This checks if the contents of a CSV column is empty, which for the purposes
-- of this application, means it is null or is either a carriage return or
-- newline character.
isEmptyCell :: T.Text -> Bool
isEmptyCell txt =
  L.any (\fn -> fn txt) $
    [ T.null
    , (== "\r")
    , (== "\n")
    ]

fields :: Set.Set T.Text
fields = Set.fromList
  [ "Equipment"
  ]

factionMap :: Map.Map T.Text Faction
factionMap = Map.fromList $ (\f -> (T.toUpper $ factionText f, f)) <$> factions

tryParseFaction :: T.Text -> Maybe (Either Faction CompendiumDetails)
tryParseFaction = fmap Left . flip Map.lookup factionMap

-- At this point, we've confirmed the following:
--   - The line is indicating a faction or compendium title change
--   - The line is NOT indicating a faction change
--
-- In that case, it _must_ be a compendium title change.
tryParseContent :: T.Text -> Maybe (Either Faction CompendiumDetails)
tryParseContent = Just . Right . mkCompendiumDetails
