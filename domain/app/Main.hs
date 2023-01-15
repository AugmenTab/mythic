module Main
  ( main
  ) where

import           Flipstone.Prelude
import           Data.Types

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Vector
import qualified System.IO as IO

main :: IO ()
main = do
  fileData <- IO.readFile "data/equipment.csv"
  case CSV.decodeByName $ LBS.fromStrict $ TE.encodeUtf8 $ T.pack fileData of
    Left err -> IO.putStrLn err
    Right x  -> printSuccess x

printSuccess :: (a, Vector RawEquipment) -> IO ()
printSuccess _ = IO.putStrLn "Success!"
