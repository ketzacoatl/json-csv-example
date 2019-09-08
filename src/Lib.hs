{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    ) where


import           Data.Aeson (eitherDecode, FromJSON, ToJSON)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Csv
  ( encodeDefaultOrderedByName
  , DefaultOrdered(headerOrder)
  , Header
  , namedRecord
  , ToNamedRecord(toNamedRecord)
  , (.=)
  )

import qualified Data.Csv as CSV
import qualified Data.Foldable as Foldable
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Control.Monad.IO.Class (liftIO)


csvHeader :: CSV.Header
csvHeader =
  Vector.fromList
    [ "Qux"
    , "Bar"
    , "Foobar"
    ]

data JsonItem = JsonItem
  { foo :: Int
  , bar :: Text
  } deriving (Show, Generic)

--data JsonItems = JsonItems [JsonItem]
--  deriving (Show, Generic)

data CsvItem = CsvItem
  { cItemQux :: Text
  , cItemBar :: Text
  , cItemFoobar :: Int
  } deriving (Show, Generic)

-- use generics
instance FromJSON JsonItem
instance ToJSON   JsonItem

--instance FromJSON JsonItems
--instance ToJSON   JsonItems

instance CSV.DefaultOrdered CsvItem where
  headerOrder _ =
    CSV.header
      [ "Qux" -- .= cItemQux
      , "Bar" -- .= cItemBar
      , "Foobar" -- .= cItemFoobar
      ]

instance CSV.ToNamedRecord CsvItem where
  toNamedRecord CsvItem{..} =
    CSV.namedRecord
      [ "Qux" .= cItemQux
      , "Bar" .= cItemBar
      , "Foobar" .= cItemFoobar
      ]

-- a little error handling when processing the JSON input data
processJsonData :: Either a b -> b
processJsonData (Left _) = error "unable to parse data"
processJsonData (Right x) = x

generateCsvItem :: JsonItem -> CsvItem
generateCsvItem i = CsvItem
  { cItemQux = "QUX"
  , cItemBar = "BAR"
  , cItemFoobar = 10
  }

-- IMPLEMENT
generateCsvItems :: [JsonItem] -> Vector CsvItem
generateCsvItems items = Vector.fromList $ generateCsvItemList items
  where generateCsvItemList items = map generateCsvItem items
--    where generateCsvItem item = do
    

encodeCsvItems :: Vector CsvItem -> ByteString
encodeCsvItems = encodeDefaultOrderedByName . Foldable.toList

writeCsvItemsToFile :: FilePath -> Vector CsvItem -> IO ()
writeCsvItemsToFile filePath =
  BS.writeFile filePath . encodeCsvItems

-- read, process, and print out info from the data.json
someFunc :: IO ()
someFunc = do
  f <- BS.readFile "data.json"
  print f
  let d = processJsonData (eitherDecode f :: Either String [JsonItem])
  print d
  let csvItems = generateCsvItems d
  writeCsvItemsToFile "items.csv" csvItems
--let jsonData = decode f :: Maybe JsonList
--putStrLn (encode jsonData)
--let j = eitherDecode f
--show $ liftIO $ Just j
  --let j1 = JsonItem $ head j
  --show j1
