module Day6.Solution (task1, task2) where

import qualified Data.List as List
import Data.Function ((&))

import qualified Utils

task1 :: IO (Maybe Int)
task1 = do
  input <- loadData
  pure $ findPacketSeparatorPosition input

findPacketSeparatorPosition :: String -> Maybe Int
findPacketSeparatorPosition str =
  List.zipWith4 (\a b c d -> [a, b, c, d]) str (drop 1 str) (drop 2 str) (drop 3 str)
        & map List.nub
        & zip [4..]
        & List.find ((== 4) . List.length . snd)
        & fmap fst

task2 :: IO (Maybe Int)
task2 = do
  input <- loadData
  pure $ findMessageStartPosition 14 input

findMessageStartPosition :: Int -> String -> Maybe Int
findMessageStartPosition uniqueSize str = go uniqueSize groups
  where
    groups :: [String]
    groups = map (flip drop str) [0 .. (uniqueSize - 1)]

    go :: Int -> [String] -> Maybe Int
    go currentPosition offsetedStringVersions =
      case (mapM Utils.head offsetedStringVersions, mapM Utils.tail offsetedStringVersions) of
        (Just heads, mTails) -> if isMessageStart heads then Just currentPosition else mTails >>= go (currentPosition + 1)
        _ -> Nothing
    
    isMessageStart :: String -> Bool
    isMessageStart str = List.length (List.nub str) == uniqueSize

loadData :: IO String
loadData = do
  contents <- Utils.loadFile "src/Day6/data.txt"
  case Utils.head $ lines contents of
    Just line -> pure line
    Nothing -> fail "Invalid data.txt file"
