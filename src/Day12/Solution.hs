module Day12.Solution (task1, task2) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP.Char
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Utils
import Utils (Parser)
import qualified Day12.Grid as Grid
import Day12.Grid (Grid)

startChar :: Char
startChar = 'S'

targetChar :: Char
targetChar = 'E'

task2 :: IO (Maybe Int)
task2 = do
  grid <- Utils.loadAndParseFile "src/Day12/data.txt" gridParser
  let mayTarget = Grid.findWithPosition (== targetChar) grid
      path = case mayTarget of
        Just (target, _) -> findShortestPathBFS target (isTarget grid) (validNeighbours grid isValidMove)
        _ -> Nothing
  pure (fmap length path)
  where
    isValidMove :: Char -> Char -> Bool
    isValidMove fromChar toChar = (fromEnum (normalizeChar fromChar) - fromEnum (normalizeChar toChar)) <= 1
    
    isTarget :: Grid Char -> (Int, Int) -> Bool
    isTarget grid pos = Maybe.fromMaybe False $ do
      char <- Grid.lookup pos grid
      pure (normalizeChar char == 'a')

task1 :: IO (Maybe Int)
task1 = do
  grid <- Utils.loadAndParseFile "src/Day12/data.txt" gridParser
  let mayStart = Grid.findWithPosition (== startChar) grid
      mayTarget = Grid.findWithPosition (== targetChar) grid
      path = case (mayStart, mayTarget) of
        (Just (start, _), Just (target, _)) -> findShortestPathBFS start (== target) (validNeighbours grid isValidMove)
        _ -> Nothing
  pure (fmap length path)
  where
    isValidMove :: Char -> Char -> Bool
    isValidMove fromChar toChar = (fromEnum (normalizeChar toChar) - fromEnum (normalizeChar fromChar)) <= 1
    
normalizeChar :: Char -> Char
normalizeChar c | c == startChar = 'a'
                | c == targetChar = 'z'
                | otherwise = c
      
validNeighbours :: Grid Char -> (Char -> Char -> Bool) -> (Int, Int) -> [(Int, Int)]
validNeighbours grid isValidMove pos = map fst $ filter (predicate . snd) (Grid.neighbours pos grid)
  where
    predicate :: Char -> Bool
    predicate = Maybe.fromMaybe False . (isValidMove <$> mayThisChar <*>) . pure

    mayThisChar :: Maybe Char
    mayThisChar = Grid.lookup pos grid

findShortestPathBFS :: (Int, Int) -> ((Int, Int) -> Bool) -> ((Int, Int) -> [(Int, Int)]) -> Maybe [(Int, Int)]
findShortestPathBFS start isTarget getNeighbours = findPath target []
  where
    (target, prevPoints) = go [start] mempty

    findPath :: (Int, Int) -> [(Int, Int)] -> Maybe [(Int, Int)]
    findPath current acc  | current == start = Just acc
                          | otherwise = case Map.lookup current prevPoints of
                                            Just next -> findPath next (next : acc)
                                            Nothing -> Nothing

    go :: [(Int, Int)] -> Map (Int, Int) (Int, Int) -> ((Int, Int), Map (Int, Int) (Int, Int))
    go [] results = ((0, 0), results)
    go toProcess results  | Just t <- List.find isTarget toProcess = (t, results)
                          | otherwise = go newToProcess newResults
      where 
        newEntries = toProcess
          >>= (\loc -> map (, loc) (getNeighbours loc))
          & filter (\(loc, _) -> Map.notMember loc results && loc /= start)
        
        newResults = Map.union results (Map.fromList newEntries)

        newToProcess = List.nub (map fst newEntries)

gridParser :: Parser (Grid Char)
gridParser = fmap Grid.fromLists $ MP.many lineParser
  where
    lineParser :: Parser [Char]
    lineParser = do
      chars <- MP.many charParser
      _ <- MP.Char.newline
      pure chars
    
    charParser :: Parser Char
    charParser = MP.choice
      [ MP.Char.lowerChar
      , MP.Char.char startChar
      , MP.Char.char targetChar
      ]
