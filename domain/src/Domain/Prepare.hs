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

data Context =
  Context
    { sheetHeader   :: Request.SheetHeader
    , faction       :: Faction
    , content       :: CompendiumDetails
    , currentSheet  :: Request.SheetLines
    , compendiumMap :: CompendiumMap T.Text
    }

prepareSheet :: Request.SheetSubject
             -> Request.SheetLines
             -> Either T.Text (CompendiumMap T.Text)
prepareSheet subject [] =
  Left $ "No content in " <> Request.sheetSubjectText subject

prepareSheet subject (header:lines) =
  let startingContent =
        case subject of
          Request.ArmorSheet        -> startingArmor
          Request.EquipmentSheet    -> startingEquipment
          Request.MeleeWeaponSheet  -> startingMelee
          Request.RangedWeaponSheet -> startingRanged

   in Right $
       separatePacks lines $
         Context
           -- We have to specify the starting Faction here because the sheets
           -- only specify a change in faction or compendium title for all the
           -- items _after_ the first, so we don't have access to this
           -- information for the first pack.
           { faction       = UNSC
           , sheetHeader   = Request.SheetHeader header
           , content       = startingContent
           , currentSheet  = []
           , compendiumMap = Map.empty
           }

separatePacks :: [T.Text] -> Context -> CompendiumMap T.Text
separatePacks [] ctx =
  updateContextCompendiumMap ctx

separatePacks (l1:[]) ctx =
  updateContextCompendiumMap $
    ctx { currentSheet = l1 : currentSheet ctx
        }

separatePacks (l1:l2:lines) ctx =
  case (tryParseFactionOrContent l1, tryParseFactionOrContent l2) of
    (Just (Left f), Just (Right c)) ->
      separatePacks lines $
        ctx { faction       = f
            , content       = c
            , currentSheet  = []
            , compendiumMap = updateContextCompendiumMap ctx
            }

    (Just (Right c), _) ->
      separatePacks (l2 : lines) $
        ctx { content       = c
            , currentSheet  = []
            , compendiumMap = updateContextCompendiumMap ctx
            }

    _ ->
      separatePacks (l2 : lines) $
        ctx { currentSheet = l1 : currentSheet ctx
            }

updateContextCompendiumMap :: Context -> CompendiumMap T.Text
updateContextCompendiumMap ctx =
  Map.insert
    (faction ctx, content ctx)
    (T.unlines $ (Request.unSheetHeader $ sheetHeader ctx) : currentSheet ctx)
    (compendiumMap ctx)

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

tryParseFaction :: T.Text -> Maybe (Either Faction CompendiumDetails)
tryParseFaction = fmap Left . flip Map.lookup factionMap

-- At this point, we've confirmed the following:
--   - The line is indicating a faction or compendium title change
--   - The line is NOT indicating a faction change
--
-- In that case, it _must_ be a compendium title change.
tryParseContent :: T.Text -> Maybe (Either Faction CompendiumDetails)
tryParseContent = Just . Right . mkCompendiumDetails

---
-- Constants
---
factionMap :: Map.Map T.Text Faction
factionMap = Map.fromList $ (\f -> (T.toUpper $ factionText f, f)) <$> factions

fields :: Set.Set T.Text
fields = Set.fromList
  [ "Equipment"
  ]

-- These are required for the same reason that the starting Faction (currently
-- defaulted to UNSC in the call to `separatePacks` in `prepareSheet`): the
-- sheets only specify a change in faction or compendium title for all the items
-- _after_ the first, so we don't have access to this information for the first
-- pack.
startingArmor :: CompendiumDetails
startingArmor = mkCompendiumDetails "" -- TODO

startingEquipment :: CompendiumDetails
startingEquipment = mkCompendiumDetails "HELMET AND FACIAL EQUIPMENT"

startingMelee :: CompendiumDetails
startingMelee = mkCompendiumDetails "" -- TODO

startingRanged :: CompendiumDetails
startingRanged = mkCompendiumDetails "" -- TODO
