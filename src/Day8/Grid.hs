{-# LANGUAGE ScopedTypeVariables #-}

module Day8.Grid (
  fromLines,
  mapAccumRowsL,
  mapAccumRowsR,
  mapAccumColumnsTop,
  mapAccumColumnsBottom,
  Grid,
  zipWith,
  foldLinesTopLeft,
  map,
  RowSplit (..),
  ColumnSplit (..),
) where

import Prelude hiding (zipWith, map)
import qualified Data.List as List
import Data.Functor ((<&>))
import Data.Function ((&))

import qualified Utils

newtype Grid a = MkGrid ((Int, Int), [[a]]) -- list of columns

instance Show a => Show (Grid a) where
  show (MkGrid (dimensions, lists)) =
    ("dimensions: " ++ show dimensions ++ "\n") ++
    concat (List.map (\l -> concatMap (\x -> show x ++ " ") l ++ "\n") (List.transpose lists))

fromLines :: [[a]] -> Maybe (Grid a)
fromLines [] = Nothing
fromLines initialLists = go lists (0, []) <&> \(colsCount, cols) -> MkGrid ((colsCount, columnSize), reverse $ cols)
  where
    lists = List.transpose initialLists

    columnSize :: Int
    columnSize = length $ head lists

    go :: [[a]] -> (Int, [[a]]) -> Maybe (Int, [[a]])
    go [] res = Just res
    go (col : resCols) (colsCount, cols) = if length col /= columnSize
      then Nothing
      else go resCols (colsCount + 1, col : cols)

mapAccumRowsL :: forall a acc b. (acc -> a -> (acc, b)) -> acc -> Grid a -> Grid b
mapAccumRowsL fnc initialAcc (MkGrid (dimensions, grid)) = MkGrid (dimensions, List.map reverse $ go grid (takeColumns $ repeat []))
  where
    takeColumns = take (fst dimensions)

    go :: [[a]] -> [[b]] -> [[b]]
    go [] results = results
    go cols results   | all null cols = results
                      | otherwise =
                        case (mapM Utils.head cols, mapM Utils.tail cols) of
                          (Just heads, Just tails) ->
                            let
                              (_, newElems) = List.mapAccumL fnc initialAcc heads
                            in
                              go tails $ List.zipWith (:) newElems results
                          _ -> error "Invariant violated, grid with different lengths of rows."

mapAccumRowsR :: (acc -> a -> (acc, b)) -> acc -> Grid a -> Grid b
mapAccumRowsR fnc acc grid =
  grid
    & reverseGrid
    & mapAccumRowsL fnc acc
    & reverseGrid
  where
    reverseGrid :: Grid a -> Grid a
    reverseGrid (MkGrid (dimensions, lists)) = MkGrid (dimensions, reverse lists)

mapAccumColumnsTop :: (acc -> a -> (acc, b)) -> acc -> Grid a -> Grid b
mapAccumColumnsTop fnc initialAcc (MkGrid (dimensions, lists)) = MkGrid (dimensions, newLists)
  where
    newLists = List.map (snd . List.mapAccumL fnc initialAcc) lists

mapAccumColumnsBottom :: (acc -> a -> (acc, b)) -> acc -> Grid a -> Grid b
mapAccumColumnsBottom fnc initialAcc (MkGrid (dimensions, lists)) = MkGrid (dimensions, newLists)
  where
    newLists = List.map (snd . List.mapAccumR fnc initialAcc) lists

zipWith :: forall a b c. (a -> b -> c) -> Grid a -> Grid b -> Maybe (Grid c)
zipWith zipper (MkGrid (dimensions1, lists1)) (MkGrid (dimensions2, lists2)) =
  if dimensions1 /= dimensions2
    then Nothing
    else lists2
      & List.zipWith (List.zipWith zipper) lists1
      & \lsts -> Just $ MkGrid (dimensions1, lsts)

foldLinesTopLeft :: (acc -> a -> acc) -> acc -> Grid a -> acc
foldLinesTopLeft folder initialAcc (MkGrid (_, lists)) = foldl (foldl folder) initialAcc lists

newtype ColumnSplit a = ColumnSplit ([a], a, [a]) deriving Show
newtype RowSplit a = RowSplit ([a], a, [a]) deriving Show

map :: forall a b. (a -> ColumnSplit a -> RowSplit a -> b) -> Grid a -> Grid b
map mapper (MkGrid (dimensions, lists)) = MkGrid (dimensions, go lists emptyRow emptyRow)
  where
    emptyRow = List.map (const []) lists

    go :: [[a]] -> [[a]] -> [[b]] -> [[b]]
    go rows visitedRows res =
      case (heads, tails) of
        (Just row, Just restRows) -> go restRows (List.zipWith (:) row visitedRows) (List.zipWith (:) (mapRow row [] visitedRows restRows []) res)
        _                         -> List.map List.reverse res
      where
        heads = mapM Utils.head rows
        tails = mapM Utils.tail rows

        mapRow :: [a] -> [a] -> [[a]] -> [[a]] -> [b] -> [b]
        mapRow [] _ _ _ acc = List.reverse acc
        mapRow _ _ [] _ _ = error "Invalid grid"
        mapRow _ _ _ [] _ = error "Invalid grid"
        mapRow (x : restRow) prevRow (prevColPart : restPrevColParts) (nextColPart : restNextColParts) acc =
          mapRow restRow (x : prevRow) restPrevColParts restNextColParts (mapper x columnSplit rowSplit : acc)
          where
            columnSplit = ColumnSplit (prevColPart, x, nextColPart)
            rowSplit = RowSplit (prevRow, x, restRow)
