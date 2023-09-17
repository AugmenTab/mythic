module Domain.Macros
  ( mkMacroCompendium
  ) where

import           Flipstone.Prelude
import           Domain.Convert.Compendium (toCompendium)
import           Data.Types.Compendium
import           Data.Types.Foundry
import           Data.Types.Prelude

import qualified Control.Concurrent.Async as Async
import           Control.Monad (filterM)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified System.Directory as Directory
import           System.FilePath ((</>))

mkMacroCompendium :: IO [Compendium FoundryData]
mkMacroCompendium = do
  let toFilepath fp = macrosFilepath </> fp
      toEntry fp =
        fmap (mkMacroCompendiumEntry fp)
          . IO.readFile
          $ toFilepath fp

  IO.putStrLn $ "Converting Macros..."
  fmap (toCompendium . Map.singleton (Nothing, mkCompendiumDetails "Macros")) $
    Async.mapConcurrently toEntry
      =<< filterM (Directory.doesFileExist . toFilepath)
      =<< Directory.listDirectory macrosFilepath

mkMacroCompendiumEntry :: String -> T.Text -> FoundryData
mkMacroCompendiumEntry fp rawMacro =
  let name =
        mkName
          . T.toTitle
          . T.unwords
          . fmap T.strip
          . T.split (== '-')
          . T.dropEnd 4
          $ T.pack fp

   in FoundryMacro $
        Macro
          { macroName = name
          , macroCommand = rawMacro
          }

macrosFilepath :: String
macrosFilepath = "../macros/"
