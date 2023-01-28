module Domain.Persist
  ( writeCompendium
  ) where

import           Flipstone.Prelude
import           Data.Types
import           Domain.JSON

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as IO

writeCompendium :: Compendium FoundryData -> IO ()
writeCompendium compendium = do
  IO.writeFile ("../" <> compendiumPath compendium)
    . T.unlines
    . fmap encodeLine
    . L.sortOn entryName
    $ compendiumEntries compendium
