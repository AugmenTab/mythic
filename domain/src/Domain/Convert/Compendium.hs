module Domain.Convert.Compendium
  ( toCompendium
  ) where

import           Flipstone.Prelude
import           Data.Types

import qualified Data.List.Extra as L
import           Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import           Data.Tuple (fst, uncurry)

toCompendium :: CompendiumEntry entry
             => CompendiumMap [entry] -> [Compendium entry]
toCompendium = mapMaybe (uncurry mkCompendium) . Map.toList

mkCompendium :: CompendiumEntry entry
             => CompendiumDetails -> [entry] -> Maybe (Compendium entry)
mkCompendium content fData = do
  let label   = mkCompendiumLabel content
      name    = mkCompendiumName  content
      entries =
        (\entry -> mkEntry label entry . Just . mkFolder $ filed entry)
          <$> fData

  cType <- entryType . fst <$> L.uncons entries

  Just
    $ Compendium
        { compendiumName    = name
        , compendiumLabel   = label
        , compendiumPath    = mkCompendiumPath name
        , compendiumType    = cType
        , compendiumEntries = entries
        }

mkEntry :: CompendiumEntry entry
        => Label -> entry -> Maybe Folder -> Entry entry
mkEntry label entry mbFolder =
  let itemLabel = mkItemLabel label $ named entry
      itemEntries = (\item -> mkEntry itemLabel item Nothing) <$> items entry
   in Entry
        { entryId     = mkEntryID label $ named entry
        , entryName   = named entry
        , entryImg    = imged entry
        , entryType   = typed entry
        , entryData   = entry
        , entryToken  = token entry
        , entryItems  = itemEntries
        , entryFolder = mbFolder
        }
