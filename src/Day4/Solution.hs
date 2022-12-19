module Day4.Solution (task1, task2) where

import Data.Maybe
import Data.Function
import Text.Read

import qualified Utils

type Assignment = (Int, Int)
type Pair = (Assignment, Assignment)

task1 :: IO Int
task1 = do
  pairs <- loadData
  pairs
    & filter oneContainsOther
    & length
    & pure

oneContainsOther :: Pair -> Bool
oneContainsOther ((start1, end1), (start2, end2)) =
  (start1 >= start2 && end1 <= end2) ||
  (start2 >= start1 && end2 <= end1)

task2 :: IO Int
task2 = do
  pairs <- loadData
  pairs
    & filter overlaps
    & length
    & pure

overlaps :: Pair -> Bool
overlaps assignment@((start1, end1), (start2, end2)) =
  oneContainsOther assignment ||
  (start1 >= start2 && start1 <= end2) ||
  (end1 >= start2 && end1 <= end2)

loadData :: IO [Pair]
loadData = do
  contents <- Utils.loadFile "src/Day4/data.txt"
  contents
    & lines
    & map readPair
    & catMaybes
    & pure

readPair :: String -> Maybe Pair
readPair str = (,) <$> readAssignment firstAssignment <*> readAssignment secondAssignment
  where
    (firstAssignment, secondAssignment) = Utils.splitOn ',' str

readAssignment :: String -> Maybe Assignment
readAssignment str = (,) <$> readMaybe start <*> readMaybe end
  where
    (start, end) = Utils.splitOn '-' str
