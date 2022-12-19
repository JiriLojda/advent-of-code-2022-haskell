{-# LANGUAGE NamedFieldPuns #-}

module Day5.Solution (task1, task2) where

import Data.List(foldl')
import qualified Data.Map as Map
import qualified Text.Megaparsec as MParsec
import qualified Text.Megaparsec.Char as MParsec.Char
import qualified Text.Megaparsec.Char.Lexer as MParsec.Char.Lexer
import qualified Text.Megaparsec.Error as MParsec.Error
import Data.Void (Void)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Function ((&))

import qualified Utils

type Crate = Char
type Stack = [Crate]
type Stacks = Map.Map Int Stack

data Move = MkMove
  { amount :: Int
  , from :: Int
  , to :: Int
  } deriving Show

task1 :: IO String
task1 = do
  (stacks, moves) <- loadData
  foldl' (processMove ((++) . reverse)) stacks moves
    & Map.elems
    & map Utils.head
    & catMaybes
    & pure

task2 :: IO String
task2 = do
  (stacks, moves) <- loadData
  foldl' (processMove (++)) stacks moves
    & Map.elems
    & map Utils.head
    & catMaybes
    & pure
  

processMove :: (Stack -> Stack -> Stack) -> Stacks -> Move -> Stacks
processMove addCrates stacks MkMove {from, to, amount} =
  case cratesToMove of
    Just crates -> Map.adjust (drop amount) from $ Map.adjust (addCrates crates) to stacks
    Nothing -> stacks
  where
    cratesToMove = fmap (take amount) $ Map.lookup from stacks

loadData :: IO (Stacks, [Move])
loadData = do
  contents <- Utils.loadFile "src/Day5/data.txt"
  case MParsec.parse dataParser "" contents of
    Left err -> fail $ MParsec.Error.errorBundlePretty err
    Right (stacks, moves) -> do
      case createStacks stacks of
        Nothing -> fail "invalid stacks..."
        Just r -> pure (r, moves)

type Parser = MParsec.Parsec Void String

dataParser :: Parser ([[Maybe Crate]], [Move])
dataParser = do
  stacks <- stacksParser
  _ <- MParsec.skipManyTill (MParsec.choice [MParsec.Char.spaceChar, MParsec.Char.digitChar, MParsec.Char.newline]) (MParsec.count 2 MParsec.Char.newline)
  moves <- moveParser `MParsec.sepEndBy` MParsec.Char.newline
  pure (stacks, moves)

createStacks :: [[Maybe Crate]] -> Maybe Stacks
createStacks initialCrates = fmap Map.fromList $ go [] 1 initialCrates
  where
    go :: [(Int, Stack)] -> Int -> [[Maybe Crate]] -> Maybe [(Int, Stack)]
    go acc _ [] = Just acc
    go acc currentIndex crates  | all null crates = Just acc
                                | otherwise =
      case (takeFirsts crates, takeTails crates) of
        (Just firstStack, rest) -> go ((currentIndex, firstStack) : acc) (currentIndex + 1) rest
        _ -> Nothing

    takeFirsts :: [[Maybe Crate]] -> Maybe [Crate]
    takeFirsts crates =
      crates
        & mapM Utils.head
        & fmap catMaybes
    
    takeTails crates = map (fromMaybe [] . Utils.tail) crates

stacksParser :: Parser [[Maybe Crate]]
stacksParser = do
  MParsec.many cratesLine
  -- case createStacks crates of
  --   Just res -> pure res
  --   Nothing -> fail "Invalid stacks..."
  where
    maybeCrateParser :: Parser (Maybe Crate)
    maybeCrateParser = MParsec.choice
      [ fmap Just crateParser
      , fmap (const Nothing) $ MParsec.Char.string "   "
      ]
    
    cratesLine :: Parser [Maybe Crate]
    cratesLine = do
      crates <- maybeCrateParser `MParsec.sepBy` MParsec.Char.char ' '
      _ <- MParsec.Char.newline
      pure crates

crateParser :: Parser Crate
crateParser = do
  _ <- MParsec.Char.char '['
  crate <- MParsec.Char.upperChar
  _ <- MParsec.Char.char ']'
  pure crate

moveParser :: Parser Move
moveParser = do
  _ <- MParsec.Char.string "move "
  amount <- MParsec.Char.Lexer.decimal
  _ <- MParsec.Char.string " from "
  from <- MParsec.Char.Lexer.decimal
  _ <- MParsec.Char.string " to "
  to <- MParsec.Char.Lexer.decimal
  pure MkMove { amount, from, to }
