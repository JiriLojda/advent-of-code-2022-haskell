{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day21.Solution (
  task1,
  task2,
) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP.Char
import Text.Megaparsec.Char.Lexer qualified as MP.Char.L
import Utils (Parser, loadAndParseFile)

task2 :: IO String
task2 = pure "Not Implemented"

task1 :: IO (Maybe Int)
task1 = do
  monkies <- Utils.loadAndParseFile "src/Day21/data.txt" monkiesParser
  pure (evaluateMonkey monkies (MkMonkeyId "root"))

evaluateMonkey :: Monkies -> MonkeyId -> Maybe Int
evaluateMonkey monkies monkeyId = do
  monkey <- Map.lookup monkeyId monkies
  case monkey of
    Constant x -> pure x
    BinOp op monkeyId1 monkeyId2 -> do
      res1 <- evaluateMonkey monkies monkeyId1
      res2 <- evaluateMonkey monkies monkeyId2
      pure $ evaluateOperation op res1 res2

evaluateOperation :: BinaryOperation -> Int -> Int -> Int
evaluateOperation op = case op of
  Sum -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div

-- Types

newtype MonkeyId = MkMonkeyId String deriving (Eq, Hashable)

type Monkies = HashMap MonkeyId Monkey

data BinaryOperation = Sum | Sub | Mul | Div

data Monkey
  = Constant Int
  | BinOp BinaryOperation MonkeyId MonkeyId

-- Parsers

monkiesParser :: Parser Monkies
monkiesParser = Map.fromList <$> monkeyEntryParser `MP.sepEndBy` MP.Char.newline
 where
  monkeyEntryParser :: Parser (MonkeyId, Monkey)
  monkeyEntryParser = do
    monkeyId <- monkeyIdParser
    _ <- MP.Char.char ':'
    _ <- MP.Char.space
    monkey <- monkeyParser
    pure (monkeyId, monkey)

monkeyParser :: Parser Monkey
monkeyParser =
  MP.choice
    [ Constant <$> MP.Char.L.decimal
    , binOpParser
    ]
 where
  binOpParser :: Parser Monkey
  binOpParser = do
    firstId <- monkeyIdParser
    _ <- MP.Char.space
    op <-
      MP.choice
        [ MP.Char.char '+' >> pure Sum
        , MP.Char.char '-' >> pure Sub
        , MP.Char.char '*' >> pure Mul
        , MP.Char.char '/' >> pure Div
        ]
    _ <- MP.Char.space
    BinOp op firstId <$> monkeyIdParser

monkeyIdParser :: Parser MonkeyId
monkeyIdParser = MkMonkeyId <$> MP.some MP.Char.lowerChar
