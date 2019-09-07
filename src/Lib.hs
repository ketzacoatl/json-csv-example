module Lib
    ( someFunc
    ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B

someFunc :: IO ()
someFunc = do
  d <- B.readFile "data.json"
  print d
