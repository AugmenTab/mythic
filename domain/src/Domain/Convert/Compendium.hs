module Domain.Convert.Compendium
  ( toCompendium
  ) where

import           Flipstone.Prelude
import           Data.Types

import qualified Data.List.Extra as L
import           Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import           Data.Tuple (fst, uncurry)

toCompendium :: CompendiumEntry item
             => CompendiumMap [item] -> [Compendium item]
toCompendium = mapMaybe (uncurry mkCompendium) . Map.toList

mkCompendium :: CompendiumEntry item
             => CompendiumData -> [item] -> Maybe (Compendium item)
mkCompendium (mbFaction, content) fData = do
  let label   = mkCompendiumLabel mbFaction content
      name    = mkCompendiumName  mbFaction content
      entries = mkEntry label <$> fData

  cType <- entryType . fst <$> L.uncons entries

  Just
    $ Compendium
        { compendiumName    = name
        , compendiumLabel   = label
        , compendiumPath    = mkCompendiumPath name
        , compendiumType    = cType
        , compendiumEntries = entries
        }

mkEntry :: CompendiumEntry entry => Label -> entry -> Entry entry
mkEntry label entry =
  Entry
    { entryId    = mkEntryID label $ named entry
    , entryName  = named entry
    , entryImg   = imged entry
    , entryType  = typed entry
    , entryData  = entry
    , entryToken = Nothing -- TODO
    , entryItems = []
    }
