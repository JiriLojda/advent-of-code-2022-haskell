module Day14.RocksMap (RocksMap(..)) where

import qualified Data.Set as Set

import qualified Day14.SimpleRocksMap as Simple
import qualified Day14.LimitedRocksMap as Limited
import Day14.CommonTypes (Position, Column)

class RocksMap a where
  fromEntries :: [(Int, Column)] -> a
  findGTInColumn :: Position -> a -> Maybe Int
  contains :: Position -> a -> Bool

instance RocksMap Simple.SimpleRocksMap where
  fromEntries = Simple.fromEntries
  findGTInColumn = Simple.findGTInColumn
  contains = Simple.contains
  
instance RocksMap Limited.LimitedRocksMap where
  fromEntries = Limited.fromEntries
  findGTInColumn = Limited.findGTInColumn
  contains = Limited.contains
