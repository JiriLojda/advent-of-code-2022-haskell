{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Day11.Solution (task1, task2) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP.Char
import qualified Text.Megaparsec.Char.Lexer as MP.Char.L
import qualified Data.Map.Strict as Map
import Data.Function ((&))
import qualified Data.List as List
import Data.Int (Int64)

import qualified Utils
import Utils (Parser)

data Monkey = MkMonkey 
  { items :: [Int64]
  , changeWorryLevel :: Int64 -> Int64
  , chooseMonkeyToThrow :: Int64 -> Int
  , inspectedItems :: Int
  , dividesBy :: Int64
  }

type Monkeys = Map.Map Int Monkey

task2 :: IO Int
task2 = do
  initialMonkeys <- Utils.loadAndParseFile "src/Day11/data.txt" monkeysParser
  let allKeys = Map.keys initialMonkeys
      commonMultiple :: Int64
      commonMultiple = product . map (.dividesBy) . Map.elems $ initialMonkeys
      finalMonkeys = List.foldl' (\m _ -> performTurn (`mod` commonMultiple) allKeys m) initialMonkeys ([1..10000] :: [Int])
  pure (sumMostActiveMonkeys 2 finalMonkeys)

task1 :: IO Int
task1 = do
  initialMonkeys <- Utils.loadAndParseFile "src/Day11/data.txt" monkeysParser
  let allKeys = Map.keys initialMonkeys
  let finalMonkeys = List.foldl' (\m _ -> performTurn boredWorry allKeys m) initialMonkeys ([1..20] :: [Int])
  pure (sumMostActiveMonkeys 2 finalMonkeys)
  where
    boredWorry :: Int64 -> Int64
    boredWorry w = w `div` 3

sumMostActiveMonkeys :: Int -> Monkeys -> Int
sumMostActiveMonkeys numMonkeys monkeys =
  monkeys
    & Map.elems
    & map (.inspectedItems)
    & List.sort
    & reverse
    & take numMonkeys
    & product

performTurn :: (Int64 -> Int64) -> [Int] -> Monkeys -> Monkeys
performTurn changeBoredWorry allKeys monkeys = List.foldl' (flip $ performTurnForMonkey changeBoredWorry) monkeys allKeys

performTurnForMonkey :: (Int64 -> Int64) -> Int -> Monkeys -> Monkeys
performTurnForMonkey changeBoredWorry monkeyKey !initialMonkeys =
  case Map.lookup monkeyKey initialMonkeys of
    Just m -> go m initialMonkeys
    Nothing -> initialMonkeys
  where
    go :: Monkey -> Monkeys -> Monkeys
    go monkey !monkeys  | null monkey.items = monkeys
                        | otherwise = go newMonkey newMonkeys
      where
        (newMonkey, newMonkeys) = processItemByMonkey changeBoredWorry monkeyKey monkey monkeys

processItemByMonkey :: (Int64 -> Int64) -> Int -> Monkey -> Monkeys -> (Monkey, Monkeys)
processItemByMonkey _ _ m@MkMonkey {items = []} monkeys = (m, monkeys)
processItemByMonkey changeBoredWorry monkeyKey monkey@MkMonkey {items = (item : restItems), changeWorryLevel, chooseMonkeyToThrow} monkeys =
  monkeys
    & Map.adjust (const updatedMonkey) monkeyKey
    & Map.adjust (\tM -> tM { items = tM.items ++ [worryLevel] }) targetMonkeyKey
    & (updatedMonkey,)
  where
    updatedMonkey = monkey { items = restItems, inspectedItems = monkey.inspectedItems + 1 }
    worryLevel = changeBoredWorry (changeWorryLevel item)
    targetMonkeyKey = chooseMonkeyToThrow worryLevel

monkeysParser :: Parser Monkeys
monkeysParser = fmap Map.fromList $ MP.sepBy monkeyWithNumberParser MP.Char.newline
  where
    monkeyWithNumberParser :: Parser (Int, Monkey)
    monkeyWithNumberParser = do
      _ <- MP.Char.string "Monkey"
      _ <- MP.Char.space
      monkeyNum <- MP.Char.L.decimal
      _ <- MP.Char.char ':'
      _ <- MP.Char.newline
      monkey <- monkeyParser
      pure (monkeyNum, monkey)

monkeyParser :: Parser Monkey
monkeyParser = do
  items <- lineWithIndent 1 $ do
    _ <- MP.Char.string "Starting items:"
    _ <- MP.Char.space
    MP.Char.L.decimal `MP.sepBy` (MP.Char.char ',' >> MP.Char.space)
  changeWorryLevel <- lineWithIndent 1 worryOperationParser
  indentParser
  (dividesBy, chooseMonkeyToThrow) <- chooseMonkeyToThrowParser
  pure $ MkMonkey {items, changeWorryLevel, chooseMonkeyToThrow, inspectedItems = 0, dividesBy}
  where
    indentParser = MP.Char.char ' ' >> MP.Char.char ' ' >> pure ()
    spaceParser = MP.Char.space >> pure ()

    lineWithIndent :: Int -> Parser a -> Parser a
    lineWithIndent indent parser = do
      mapM_ (const indentParser) [1..indent]
      res <- parser
      _ <- MP.Char.newline
      pure res

    worryOperationParser :: Parser (Int64 -> Int64)
    worryOperationParser = do
      _ <- MP.Char.string "Operation:"
      spaceParser
      _ <- MP.Char.string "new"
      spaceParser
      _ <- MP.Char.char '='
      spaceParser
      operand1 <- operandParser
      spaceParser
      operation <- operationParser
      spaceParser
      operand2 <- operandParser
      pure $ \old -> operation (processOperand old operand1) (processOperand old operand2)
      where
        processOperand :: Int64 -> Operand -> Int64
        processOperand old operand = case operand of
          Old -> old
          Constant int -> int

    chooseMonkeyToThrowParser :: Parser (Int64, Int64 -> Int)
    chooseMonkeyToThrowParser = do
      _ <- MP.Char.string "Test: divisible by"
      spaceParser
      divideBy <- MP.Char.L.decimal
      _ <- MP.Char.newline
      monkeyOnTrue <- lineWithIndent 2 $ do
        _ <- MP.Char.string "If true: throw to monkey"
        spaceParser
        MP.Char.L.decimal
      monkeyOnFalse <- lineWithIndent 2 $ do
        _ <- MP.Char.string "If false: throw to monkey"
        spaceParser
        MP.Char.L.decimal
      pure $ (divideBy, \worryLevel -> if worryLevel `mod` divideBy == 0 then monkeyOnTrue else monkeyOnFalse)


    
    operationParser :: Parser (Int64 -> Int64 -> Int64)
    operationParser = MP.choice
      [ MP.Char.char '+' >> pure (+)
      , MP.Char.char '-' >> pure (-)
      , MP.Char.char '*' >> pure (*)
      ]

    operandParser :: Parser Operand
    operandParser = MP.choice
      [ MP.Char.string "old" >> pure Old
      , MP.Char.L.decimal >>= (pure . Constant)
      ]
    
data Operand
  = Old
  | Constant Int64
