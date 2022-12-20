module Day14.SimpleRocksMap
  ( SimpleRocksMap
  , fromEntries
  , findGTInColumn
  , contains
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Utils
import Day14.CommonTypes (Position, Column)

newtype SimpleRocksMap = MkSimpleRocksMap (Map.Map Int Column) -- map of column number to the set of stone positions in the given column

fromEntries :: [(Int, Column)] -> SimpleRocksMap
fromEntries = MkSimpleRocksMap . Map.fromListWith Set.union

findGTInColumn :: Position -> SimpleRocksMap -> Maybe Int
findGTInColumn (x, y) (MkSimpleRocksMap columns) = Map.lookup x columns >>= Set.lookupGT y

contains :: Position -> SimpleRocksMap -> Bool
contains (x, y) (MkSimpleRocksMap columns) = fmap (Set.member y) (Map.lookup x columns) `Utils.orElse` False
