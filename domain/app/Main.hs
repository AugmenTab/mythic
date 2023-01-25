module Main
  ( main
  ) where

import           Flipstone.Prelude
import           Data.Types
import qualified Domain.Convert as Convert
import qualified Domain.Prepare as Prepare
import qualified Domain.Request as Request

import           Control.Monad (forM_)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Conduit (tlsManagerSettings)
import qualified System.IO as IO

main :: IO ()
main = do
  case Request.makeSheetRequest of
    Left  err -> IO.putStrLn $ T.unpack err
    Right req -> do
      mgr <- HTTP.newManager tlsManagerSettings
      forM_ (Map.toList Request.sheetDataMap) $ \(subject, sheetData) -> do
        let subjectTxt = T.unpack $ Request.sheetSubjectText subject

        IO.putStrLn $ "Fetching " <> subjectTxt <> "..."
        resp <- HTTP.httpLbs (Request.setSheetQueryStrings sheetData req) mgr

        IO.putStrLn $ "Converting " <> subjectTxt <> "..."
        let result = Request.responseContent resp subject
                 >>= Prepare.prepareSheet subject
                 >>= Convert.ingestRaw subject
                 >>= Convert.toFoundry
                 >>= (pure . Convert.toCompendium)

        IO.putStrLn $ "Finished with " <> subjectTxt <> "."
        handleSheetResult result

handleSheetResult :: Either Text [Compendium FoundryData] -> IO ()
handleSheetResult (Left err)        = IO.putStrLn $ T.unpack err
handleSheetResult (Right compendia) = forM_ compendia $ \compendium -> do
  IO.putStrLn $
    L.concat [ "Writing compendium "
             , T.unpack $ labelText $ compendiumLabel compendium
             , " to " <> compendiumPath compendium <> "..."
             ]

  -- TODO: >>= Persist.storeCompendia -- Write Compendia to disk
  IO.putStrLn "Done."
