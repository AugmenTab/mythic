module Main
  ( main
  ) where

import           Flipstone.Prelude
import qualified Domain.Convert as Convert
import qualified Domain.Prepare as Prepare
import qualified Domain.Request as Request

import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Traversable (for, sequence)
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Conduit (tlsManagerSettings)
import qualified System.IO as IO

main :: IO ()
main = do
  case Request.makeSheetRequest of
    Left  err -> IO.putStrLn $ T.unpack err
    Right req -> do
      mgr <- HTTP.newManager tlsManagerSettings
      result <-
        for (Map.toList Request.sheetDataMap) $ \(subject, sheetData) -> do
          let subjectTxt = T.unpack $ Request.sheetSubjectText subject

          IO.putStrLn $ "Fetching " <> subjectTxt <> "..."
          resp <- HTTP.httpLbs (Request.setSheetQueryStrings sheetData req) mgr

          IO.putStrLn $ "Converting " <> subjectTxt <> "..."
          let result = Request.responseContent resp subject -- Get response body lines
                   >>= Prepare.prepareSheet subject -- Do formatting work on sheet
                   >>= Convert.ingestRaw subject -- Ingest as Raw type
                   >>= Convert.toFoundry -- Convert from Raw type to Foundry type
                -- >>= Convert.toCompendium -- Convert from Foundry type to Compendium
                -- >>= Persist.storeCompendium -- Write Compendium to disk

          IO.putStrLn $ "Finished with " <> subjectTxt <> "."
          pure result

      case sequence result of
        Left  e -> IO.putStrLn $ T.unpack e
        Right _ -> IO.putStrLn "Done."
