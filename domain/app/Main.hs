{-# LANGUAGE ScopedTypeVariables #-}
module Main
  ( main
  ) where

import           Flipstone.Prelude
import           Data.Types

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as CSV
import           Data.Foldable (mapM_)
import           Data.List (take)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Tuple (snd)
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Conduit (tlsManagerSettings)
import           Network.HTTP.Client (httpLbs)
import qualified Network.HTTP.Simple as HTTP
import qualified System.IO as IO
import           Text.Show (show)

main :: IO ()
main = do
  fileData <-
    case makeSheetRequest of
      Left  err -> fail $ T.unpack err
      Right req -> do
        mgr <- HTTP.newManager tlsManagerSettings
        resp <- httpLbs req mgr
        pure $ id
             $ T.unlines $ take 14 $ T.lines -- remove breaking lines for now
             $ TE.decodeUtf8 $ LBS.toStrict $ HTTP.responseBody resp

  case CSV.decodeByName $ LBS.fromStrict $ TE.encodeUtf8 fileData of
    Left err -> IO.putStrLn err
    Right x  -> printSuccess x

printSuccess :: (a, V.Vector RawEquipment) -> IO ()
printSuccess = mapM_ (IO.putStrLn . show) . snd

makeSheetRequest :: Either Text HTTP.Request
makeSheetRequest =
  let baseURL = "https://docs.google.com"
      path = "/spreadsheets/d/1oebS4iy37YYpuooGVI_7WpSOCk2Fy_zh2ACC6_heLJA/export"
      queryStrings =
        [ ("format", Just "csv")
        , ("gid"   , Just "515202982")
        , ("range" , Just "A1:E233")
        ]

      buildRequest =
          Right
        . HTTP.addRequestHeader "Accept" "text/csv"
        . HTTP.setQueryString queryStrings
        . HTTP.setRequestPath path
        . HTTP.setRequestSecure True

   in maybe (Left "Couldn't parse URL") buildRequest $ HTTP.parseRequest baseURL
