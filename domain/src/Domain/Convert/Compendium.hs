module Domain.Convert.Compendium
  ( toCompendium
  ) where

import           Flipstone.Prelude
import           Data.Types

import qualified Data.Map.Strict as Map
import           Data.Tuple (uncurry)

toCompendium :: CompendiumEntry item
             => CompendiumMap [item] -> [Compendium item]
toCompendium = fmap (uncurry mkCompendium) . Map.toList

mkCompendium :: CompendiumEntry item
             => CompendiumData -> [item] -> Compendium item
mkCompendium (faction, content) fData =
  let label = mkCompendiumLabel faction content
      name  = mkCompendiumName  faction content
   in Compendium
        { compendiumName    = name
        , compendiumLabel   = label
        , compendiumPath    = mkCompendiumPath name
        , compendiumEntries = mkEntry label <$> fData
        }

mkEntry :: CompendiumEntry item => Label -> item -> Entry item
mkEntry label item =
  Entry
    { entryId   = mkItemID label $ named item
    , entryName = named item
    , entryImg  = imged item
    , entryType = typed item
    , entryData = item
    }
