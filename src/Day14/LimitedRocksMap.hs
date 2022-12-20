module Day14.LimitedRocksMap
  ( LimitedRocksMap
  , fromEntries
  , findGTInColumn
  , contains
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Applicative ((<|>))
import Data.Function ((&))
import qualified Data.Maybe as Maybe

import qualified Utils
import Day14.CommonTypes (Position, Column)
import qualified Day14.SimpleRocksMap as Simple

newtype LimitedRocksMap = MkLimitedRocksMap (Int, Simple.SimpleRocksMap) -- (y height of the bottom limit, rest of the map)

fromEntries :: [(Int, Column)] -> LimitedRocksMap
fromEntries entries = MkLimitedRocksMap (maxY + 2, Simple.fromEntries entries)
  where
    !maxY =
      entries
        & map (Set.lookupMax . snd)
        & Maybe.catMaybes
        & maximum


findGTInColumn :: Position -> LimitedRocksMap -> Maybe Int
findGTInColumn (x, y) (MkLimitedRocksMap (bottom, simpleMap)) = Simple.findGTInColumn (x, y) simpleMap <|> Just bottom

contains :: Position -> LimitedRocksMap -> Bool
contains (x, y) (MkLimitedRocksMap (bottom, simpleMap)) = y == bottom || Simple.contains (x, y) simpleMap
