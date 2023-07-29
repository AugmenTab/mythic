module Main
  ( main
  ) where

import           Flipstone.Prelude
import           Data.Types
import qualified Domain.Convert as Convert
import qualified Domain.Persist as Persist
import qualified Domain.Request as Request

import qualified Control.Concurrent.Async as Async
import           Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import           Data.Tuple (uncurry)
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Conduit (tlsManagerSettings)
import qualified System.Exit as Exit

main :: IO ()
main = do
  case Request.makeSheetRequest of
    Left  err -> IO.putStrLn err
    Right req -> do
      let sheets = Map.toList Request.sheetDataMap

      mgr <- HTTP.newManager tlsManagerSettings
      compendia <-
        fmap concat
          . Async.forConcurrently sheets
          . uncurry
          $ \subject sheetData -> do
            let subjectTxt = Request.sheetSubjectText subject

            resp <-
              HTTP.httpLbs (Request.setSheetQueryStrings sheetData req) mgr

            IO.putStrLn $ "Converting " <> subjectTxt <> "..."
            either (Exit.die . T.unpack) pure $
              pure . Convert.toCompendium
                =<< Convert.toFoundry
                =<< Convert.ingestRaw subject
                =<< Request.responseContent resp subject

      handleSheetResults compendia
      Persist.writeManifest compendia
      IO.putStrLn "Done."

handleSheetResults :: [Compendium FoundryData] -> IO ()
handleSheetResults compendia = forM_ compendia $ \compendium -> do
  IO.putStrLn $
    T.unwords [ "Writing compendium"
              , labelText $ compendiumLabel compendium
              , "to"
              , T.pack $ compendiumPath compendium
              , " ..."
              ]

  Persist.writeCompendium compendium
