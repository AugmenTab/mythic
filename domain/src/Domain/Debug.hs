module Domain.Debug
  ( writeDebugFile
  ) where

import           Flipstone.Prelude
import qualified Domain.Request as Request

import qualified Data.ByteString.Lazy as LBS
import           Data.Either.Combinators (whenRight)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Network.HTTP.Client as HTTP

writeDebugFile :: HTTP.Response LBS.ByteString -> Request.SheetSubject -> IO ()
writeDebugFile resp subject =
  whenRight (Request.responseContent resp subject) $ \lines -> do
    let subjectTxt = Request.sheetSubjectText subject

    IO.putStrLn $ "Writing " <> subjectTxt <> " debug file..."
    IO.writeFile (T.unpack $ "data/" <> subjectTxt <> ".csv") $ T.unlines lines
