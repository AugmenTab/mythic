module Domain.Persist
  ( writeCompendium
  , writeManifest
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

-- This writes the list of compendia to a JSON file so the text can be copied
-- into the manifest.json file. This will allow the system to read in the .db
-- files and treat them as compendia.
--
-- In the future, this will write the entire manifest file with these packs
-- included.
writeManifest :: [Compendium FoundryData] -> IO ()
writeManifest = IO.writeFile "../packs/packs.json" . encodePage
