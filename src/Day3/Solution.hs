{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day3.Solution (task1, task2) where

import qualified Data.Set as Set
import Data.Char (ord)
import Data.Function
import Data.Maybe

import qualified Utils

data Bag = MkBag
  { firstCompartment :: Set.Set Char
  , secondCompartment :: Set.Set Char
  } deriving Show

task1 :: IO Int
task1 = do
  contents <- Utils.loadFile "src/Day3/data.txt"
  lines contents
    & map parseBag
    & map bagDuplicates
    & map (sum . map itemPriority)
    & sum
    & pure

task2 :: IO Int
task2 = do
  contents <- Utils.loadFile "src/Day3/data.txt"
  lines contents
    & map parseBag
    & groupBags
    & map findCommonItem
    & catMaybes
    & map itemPriority
    & sum
    & pure

itemPriority :: Char -> Int
itemPriority i  | i >= 'a' && i <= 'z' = ord i - 96
                | i >= 'A' && i <= 'Z' = ord i - 38
                | otherwise = 0

parseBag :: String -> Bag
parseBag line = MkBag { firstCompartment = Set.fromList firstHalf, secondCompartment = Set.fromList secondHalf }
  where
    lineLength = length line
    firstHalf = take (lineLength `div` 2) line
    secondHalf = drop (lineLength `div` 2) line

bagDuplicates :: Bag -> [Char]
bagDuplicates bag = Set.toList $ Set.intersection bag.firstCompartment bag.secondCompartment

groupBags :: [Bag] -> [[Bag]]
groupBags bags = go bags []
  where
    go :: [Bag] -> [[Bag]] -> [[Bag]]
    go [] res = res
    go (bag : restBags) [] = go restBags [[bag]]
    go (bag : restBags) (lastGroup : restGroups) = if length lastGroup == 3
      then go restBags ([bag] : lastGroup : restGroups)
      else go restBags ((bag : lastGroup) : restGroups)

findCommonItem :: [Bag] -> Maybe Char
findCommonItem [bag1, bag2, bag3] =
  Set.intersection (bagContents bag1) (bagContents bag2)
    & Set.intersection (bagContents bag3)
    & firstElement
findCommonItem _ = Nothing

firstElement :: Set.Set a -> Maybe a
firstElement set = go $ Set.toList set
  where
    go [i] = Just i
    go _ = Nothing

bagContents :: Bag -> Set.Set Char
bagContents bag = Set.union bag.firstCompartment bag.secondCompartment
