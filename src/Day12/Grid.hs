module Day12.Grid
  ( Grid
  , fromLists
  , lookup
  , neighbours
  , findWithPosition
  ) where

import Prelude hiding (lookup)
import qualified Data.Vector as Vec
import Data.Vector ((!?))
import Data.Function ((&))
import qualified Data.Maybe as Maybe

newtype Grid a = MkGrid (Vec.Vector (Vec.Vector a)) deriving Show -- vector of rows

fromLists :: [[a]] -> Grid a
fromLists lists =
  lists
    & map Vec.fromList
    & Vec.fromList
    & MkGrid

lookup :: (Int, Int) -> Grid a -> Maybe a
lookup (x, y) (MkGrid vecs) = (vecs !? y) >>= (!? x)

neighbours :: (Int, Int) -> Grid a -> [((Int, Int), a)]
neighbours (x, y) grid =
  [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    & map (\pos -> fmap (pos,) $ lookup pos grid)
    & Maybe.catMaybes

findWithPosition :: (a -> Bool) -> Grid a -> Maybe ((Int, Int), a)
findWithPosition predicate (MkGrid vecs) =
  Vec.indexed vecs
    & Vec.map (\(y, row) -> Vec.find (predicate . snd) . Vec.map (\(x, e) -> ((x, y), e)) . Vec.indexed $ row)
    & Vec.find Maybe.isJust
    & Maybe.fromMaybe Nothing
