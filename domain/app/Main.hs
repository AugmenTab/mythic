module Main
  ( main
  ) where

import           Flipstone.Prelude
import           Lib

import qualified Data.Text.IO as Text

main :: IO ()
main = do
  Text.putStrLn "Up and running"
  someFunc
