module Day14.Solution (task1, task2) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP.Char
import qualified Text.Megaparsec.Char.Lexer as MP.Char.L
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Applicative ((<|>))
import qualified Control.Monad as Monad

import qualified Utils
import Utils (Parser)
import qualified Day14.RocksMap as RocksMap
import Day14.RocksMap (RocksMap)
import Day14.SimpleRocksMap (SimpleRocksMap)
import Day14.LimitedRocksMap (LimitedRocksMap)
import Day14.CommonTypes (Position, Column)

newtype SandMap = MkSandMap (Map.Map Int Column) deriving Show -- map of column number to set of resting sand positions in the given column

task2 :: IO Int
task2 = do
  rocksMap <- Utils.loadAndParseFile "src/Day14/data.txt" (rocksMapParser :: Parser LimitedRocksMap)
  rocksMap
    & simulateSandUntilFull (500, 0) 
    & sumSandGrains
    & pure

task1 :: IO Int
task1 = do
  rocksMap <- Utils.loadAndParseFile "src/Day14/data.txt" (rocksMapParser :: Parser SimpleRocksMap)
  rocksMap
    & simulateSandUntilFull (500, 0) 
    & sumSandGrains
    & pure

sumSandGrains :: SandMap -> Int
sumSandGrains (MkSandMap sandMap) =
  sandMap
    & Map.elems
    & map Set.size
    & sum

simulateSandUntilFull :: RocksMap a => Position -> a -> SandMap
simulateSandUntilFull sourcePos rocksMap = MkSandMap (go mempty)
  where
    go :: Map.Map Int Column -> Map.Map Int Column
    go !sandMap =
      case fallUntilRest sourcePos rocksMap (MkSandMap sandMap) of
        Nothing -> sandMap
        Just newGrainPos  | newGrainPos == sourcePos -> insertGrain sourcePos sandMap
                          | otherwise -> go (insertGrain newGrainPos sandMap)
    
    insertGrain :: Position -> Map.Map Int Column -> Map.Map Int Column
    insertGrain (x, y) sandMap = Map.insertWith Set.union x (Set.singleton y) sandMap

fallUntilRest :: RocksMap a => Position -> a -> SandMap -> Maybe Position
fallUntilRest position rocksMap sandMap = do
  stopedOn <- fallUntilBlocked position rocksMap sandMap
  case switchColumn stopedOn rocksMap sandMap of
    Nothing -> Just stopedOn
    Just newPos -> fallUntilRest newPos rocksMap sandMap

fallUntilBlocked :: RocksMap a => Position -> a -> SandMap -> Maybe Position
fallUntilBlocked (x, y) rocksMap (MkSandMap sandColumns) = do
  firstBlocker <- case (sandBlocker, rockBlocker) of
    (Just s, Just r) -> Just (min s r)
    (s, r) -> s <|> r
  pure (x, firstBlocker - 1)
  where
    sandBlocker = Map.lookup x sandColumns >>= Set.lookupGT y
    rockBlocker = RocksMap.findGTInColumn (x, y) rocksMap

switchColumn :: RocksMap a => Position -> a -> SandMap -> Maybe Position
switchColumn (x, y) rocksMap (MkSandMap sandColumns) = do
  column <- leftUsableColumn <|> rightUsableColumn
  pure (column, newY)
  where
    newY = y + 1

    usableColumn :: Int -> Maybe Int
    usableColumn columnNumber = do
      sandCol <- Map.lookup columnNumber sandColumns <|> Just Set.empty
      Monad.guard (not $ RocksMap.contains (columnNumber, newY) rocksMap)
      Monad.guard (newY `Set.notMember` sandCol)
      pure columnNumber

    leftUsableColumn = usableColumn (x - 1)
    rightUsableColumn = usableColumn (x + 1)

-- Parsers

rocksMapParser :: RocksMap a => Parser a
rocksMapParser = do
  parsedLines <- lineParser `MP.sepEndBy1` MP.Char.newline
  parsedLines
    & map processLine
    & concat
    & RocksMap.fromEntries
    & pure

processLine :: [Position] -> [(Int, Column)]
processLine [] = []
processLine (firstPos : restPos) = go firstPos restPos []
  where
    go :: Position -> [Position] -> [[(Int, Column)]] -> [(Int, Column)]
    go _ [] acc = concat acc
    go (prevX, prevY) ((x, y) : rest) acc =
      if prevX == x
        then go (x, y) rest ([(x, Set.fromList [(min prevY y)..(max prevY y)])] : acc)
        else go (x, y) rest ((map (, Set.singleton y) [(min prevX x)..(max prevX x)]) : acc)

lineParser :: Parser [Position]
lineParser = positionParser `MP.sepBy` MP.Char.string " -> "

positionParser :: Parser Position
positionParser = do
  x <- MP.Char.L.decimal
  _ <- MP.Char.char ','
  y <- MP.Char.L.decimal
  pure (x, y)
