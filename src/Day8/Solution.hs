{-# LANGUAGE BangPatterns #-}

module Day8.Solution (task1, task2) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP.Char
import Data.Char (digitToInt)
import Data.Function ((&))

import qualified Utils
import Utils (Parser)
import qualified Day8.Grid as Grid
import Day8.Grid (Grid)

task2 :: IO Int
task2 = do
  grid <- Utils.loadAndParseFile "src/Day8/data.txt" gridParser
  pure $ maxScenicScore grid

maxScenicScore :: Grid Int -> Int
maxScenicScore = Grid.foldLinesTopLeft max 0 . calculateScenicScore


calculateScenicScore :: Grid Int -> Grid Int
calculateScenicScore = Grid.map scenicScoreMapper
  where
    scenicScoreInDirection :: Int -> [Int] -> Int -> Int
    scenicScoreInDirection _ [] !acc = acc
    scenicScoreInDirection height (tree : restTrees) acc  | tree >= height = acc + 1
                                                          | otherwise = scenicScoreInDirection height restTrees (acc + 1)
    
    scenicScoreMapper :: Int -> Grid.ColumnSplit Int -> Grid.RowSplit Int -> Int
    scenicScoreMapper height (Grid.ColumnSplit (top, _, bottom)) (Grid.RowSplit (left, _, right)) =
      scenicScoreInDirection height top 0 *
      scenicScoreInDirection height bottom 0 *
      scenicScoreInDirection height left 0 *
      scenicScoreInDirection height right 0

task1 :: IO (Maybe Int)
task1 = do
  grid <- Utils.loadAndParseFile "src/Day8/data.txt" gridParser
  pure $ countVisibleTrees grid

countVisibleTrees :: Grid Int -> Maybe Int
countVisibleTrees grid =
  visibleTrees
    & fmap (Grid.foldLinesTopLeft (\acc x -> if x then acc + 1 else acc) 0)
  where
    visibilityMapFolder :: Int -> Int -> (Int, Bool)
    visibilityMapFolder maxHeight currentHeight =
      (max maxHeight currentHeight, currentHeight > maxHeight)
    
    visibleFromLeft :: Grid Bool
    visibleFromLeft = Grid.mapAccumRowsL visibilityMapFolder (-1) grid
    
    visibleFromRight :: Grid Bool
    visibleFromRight = Grid.mapAccumRowsR visibilityMapFolder (-1) grid
    
    visibleFromTop :: Grid Bool
    visibleFromTop = Grid.mapAccumColumnsTop visibilityMapFolder (-1) grid
    
    visibleFromBottom :: Grid Bool
    visibleFromBottom = Grid.mapAccumColumnsBottom visibilityMapFolder (-1) grid

    visibleTrees :: Maybe (Grid Bool)
    visibleTrees =
      visibleFromLeft
        & Grid.zipWith (||) visibleFromRight
        & (>>= Grid.zipWith (||) visibleFromTop)
        & (>>= Grid.zipWith (||) visibleFromBottom)

gridParser :: Parser (Grid Int)
gridParser = do
  lists <- MP.many gridLineParser
  case Grid.fromLines lists of
    Nothing -> fail "Invalid dimensions of the input."
    Just grid -> pure grid
  where
    gridLineParser :: Parser [Int]
    gridLineParser = do
      chars <- MP.many MP.Char.digitChar
      _ <- MP.Char.newline
      pure $ map digitToInt chars
