module Main
  ( main
  ) where

import           Flipstone.Prelude
import qualified Domain.Request as Request

import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (mapM_)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
          resp <- HTTP.httpLbs (Request.setSheetQueryStrings sheetData req) mgr
          let _parse = LBS.fromStrict
                     . TE.encodeUtf8
                     . T.unlines . L.take 14 . T.lines -- remove breaking lines for now
                     . TE.decodeUtf8
                     . LBS.toStrict
                     . HTTP.responseBody


          -- TODO: Process resp into separate compendia
          pure $ const (Request.sheetSubjectText subject) <$> Right resp

      case sequence result of
        Left  err -> IO.putStrLn $ T.unpack err
        Right res -> mapM_ (IO.putStrLn . T.unpack) res
