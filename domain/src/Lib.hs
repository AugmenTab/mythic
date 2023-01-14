module Lib
  ( someFunc
  ) where

import           Flipstone.Prelude

import qualified Data.Text.IO as Text

someFunc :: IO ()
someFunc = do
  Text.putStrLn "In Lib"
