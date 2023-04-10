module Domain.Prepare
  ( prepareSheet
  ) where

import           Flipstone.Prelude
import           Data.Types
import qualified Domain.Request as Request

import qualified Data.List as L
import qualified Data.Map.Strict as Map
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
prepareSheet subject lines =
  case L.uncons lines of
    Just (header, rows) ->
      Right $
       separatePacks rows $
         Context
           -- We have to specify the starting Faction here because the sheets
           -- only specify a change in faction or compendium title for all the
           -- items _after_ the first, so we don't have access to this
           -- information for the first pack.
           { faction       = UNSC
           , sheetHeader   = Request.SheetHeader header
           , content       = mkCompendiumDetails
                           $ Request.sheetSubjectTitle subject
           , currentSheet  = []
           , compendiumMap = Map.empty
           }

    Nothing ->
      Left $ "No content in " <> Request.sheetSubjectText subject

separatePacks :: [T.Text] -> Context -> CompendiumMap T.Text
separatePacks []         ctx = updateCompendiumMap ctx
separatePacks (row:rows) ctx =
  case T.split (== ',') row of
    -- This guard checks for the following:
    --   - The first column is one of the various "empty" cell values
    --   - The second column is a valid COMP_field value
    --
    -- This typically means that this line is indicating a change in the faction
    -- (and thus the compendium) for the upcoming items. If col3 does contain a
    -- faction, we push the current compendium to the `CompendiumMap`.
    -- Otherwise, it's a blank line with no valuable data, so we skip the line
    -- entirely and move on.
    --
    -- If the line never qualified for the two checks to begin with, it's a row
    -- containing an Item, so we add it and move on.
    col1 : col2 : col3 : _cols | isEmptyCell col1, Set.member col2 fields ->
      case Map.lookup (T.toUpper col3) factionMap of
        Just newFaction ->
          separatePacks rows $
            ctx { faction       = newFaction
                , currentSheet  = []
                , compendiumMap = updateCompendiumMap ctx
                }

        Nothing ->
          separatePacks rows ctx

    _ ->
      separatePacks rows $
        ctx { currentSheet = row : currentSheet ctx
            }

updateCompendiumMap :: Context -> CompendiumMap T.Text
updateCompendiumMap ctx =
  Map.insert
    (faction ctx, content ctx)
    (T.unlines $ (Request.unSheetHeader $ sheetHeader ctx) : currentSheet ctx)
    (compendiumMap ctx)

-- This checks if the contents of a CSV column is empty, which for the purposes
-- of this application, means it is null or is either a carriage return or
-- newline character.
isEmptyCell :: T.Text -> Bool
isEmptyCell txt =
  L.any (\fn -> fn txt) $
    [ T.null
    , (== "Default")
    , (== "\r")
    , (== "\n")
    ]

---
-- Constants
---
factionMap :: Map.Map T.Text Faction
factionMap = Map.fromList $ (\f -> (T.toUpper $ factionText f, f)) <$> factions

fields :: Set.Set T.Text
fields = Set.fromList
  [ "Equipment"
  , "Mel_Wep"
  , "Ran_Wep"
  ]
