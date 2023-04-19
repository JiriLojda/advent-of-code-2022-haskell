{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Day21.Solution (
  task1,
  task2,
) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable)
import Data.Ratio (Ratio, (%))
import Data.Ratio qualified as Fraction
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP.Char
import Text.Megaparsec.Char.Lexer qualified as MP.Char.L
import Utils (Parser, loadAndParseFile)

task2 :: IO (Maybe Int)
task2 = do
  monkies <- Utils.loadAndParseFile "src/Day21/data.txt" monkiesParser
  pure $ floor <$> solveLinearEquation (MkEquationArgs{equationMonkeyId = MkMonkeyId "root", unknownMonkeyId = MkMonkeyId "humn"}) monkies

type Fraction = Ratio Int

evaluateFraction :: Fraction -> Double
evaluateFraction fr = fromIntegral (Fraction.numerator fr) / fromIntegral (Fraction.denominator fr)

data SimplificationResult
  = Const Fraction
  | UnkownWithConst {multiplier :: Fraction, offset :: Fraction}
  deriving (Show)

data EquationArgs = MkEquationArgs
  { equationMonkeyId :: MonkeyId
  , unknownMonkeyId :: MonkeyId
  }

solveLinearEquation :: EquationArgs -> Monkies -> Maybe Double
solveLinearEquation args monkies = case Map.lookup args.equationMonkeyId monkies of
  Nothing -> Nothing
  Just (Constant x) -> Just $ fromIntegral x
  Just (BinOp _ leftMonkeyId rightMonkeyId) -> do
    leftMonkey <- Map.lookup leftMonkeyId monkies
    leftRes <- simplifyMonkey args.unknownMonkeyId monkies leftMonkey
    rightMonkey <- Map.lookup rightMonkeyId monkies
    rightRes <- simplifyMonkey args.unknownMonkeyId monkies rightMonkey
    case (removeZeroUnknown leftRes, removeZeroUnknown rightRes) of
      (l@UnkownWithConst{}, r@UnkownWithConst{}) -> Just . getConst . removeMultiplier . moveOffset . sumUnknowns $ ((l.multiplier, l.offset), (r.multiplier, r.offset))
      (Const _, Const _) -> Nothing
      rest -> Just . getConst . removeMultiplier . moveOffset . getConstAndUnknown $ rest
   where
    getConst :: (Fraction, (Fraction, Fraction)) -> Double
    getConst = evaluateFraction . fst

    moveOffset :: (Fraction, (Fraction, Fraction)) -> (Fraction, (Fraction, Fraction))
    moveOffset (constant, (multiplier, offset)) = (constant - offset, (multiplier, 0))

    removeMultiplier :: (Fraction, (Fraction, Fraction)) -> (Fraction, (Fraction, Fraction))
    removeMultiplier (constant, (multiplier, offset)) = ((floor constant % 1) / multiplier, (1, offset / multiplier))

    sumUnknowns :: ((Fraction, Fraction), (Fraction, Fraction)) -> (Fraction, (Fraction, Fraction))
    sumUnknowns ((mult1, off1), (mult2, off2)) = (off1, (mult1 + mult2, off2))

    getConstAndUnknown :: (SimplificationResult, SimplificationResult) -> (Fraction, (Fraction, Fraction))
    getConstAndUnknown (Const x, unknown@(UnkownWithConst _ _)) = (floor x % 1, (unknown.multiplier, unknown.offset))
    getConstAndUnknown (unknown@(UnkownWithConst _ _), Const x) = (floor x % 1, (unknown.multiplier, unknown.offset))
    getConstAndUnknown _ = error "This function requires exactly one constant and one unknown."

    removeZeroUnknown :: SimplificationResult -> SimplificationResult
    removeZeroUnknown UnkownWithConst{multiplier = 0, offset} = Const offset
    removeZeroUnknown x = x

simplifyMonkey :: MonkeyId -> Monkies -> Monkey -> Maybe SimplificationResult
simplifyMonkey _ _ (Constant x) = Just (Const $ fromIntegral x)
simplifyMonkey unknownId monkies (BinOp op monkey1 monkey2) = do
  res1 <- simplifyArg monkey1
  res2 <- simplifyArg monkey2
  pure (applyOpOnSimplificationResult op res1 res2)
 where
  simplifyArg :: MonkeyId -> Maybe SimplificationResult
  simplifyArg monkeyId =
    if monkeyId == unknownId
      then Just (UnkownWithConst{multiplier = 1, offset = 0})
      else Map.lookup monkeyId monkies >>= simplifyMonkey unknownId monkies

data Direction = ArgLike | Reversed deriving (Show)

applyOpOnSimplificationResult :: BinaryOperation -> SimplificationResult -> SimplificationResult -> SimplificationResult
applyOpOnSimplificationResult op res1 res2 = case (res1, res2) of
  (Const x1, Const x2) -> Const (evaluateOperation op x1 x2)
  (Const x1, UnkownWithConst{multiplier, offset}) -> applyOpOnUnknown op x1 ArgLike (multiplier, offset)
  (UnkownWithConst{multiplier, offset}, Const x2) -> applyOpOnUnknown op x2 Reversed (multiplier, offset)
  _ -> error "This algorithm can only work with linear equations. There seems to be a potential for multiplication of unknowns."

applyOpOnUnknown :: BinaryOperation -> Fraction -> Direction -> (Fraction, Fraction) -> SimplificationResult
applyOpOnUnknown op constArg dir (multiplier, offset) = case op of
  Sum -> UnkownWithConst{multiplier, offset = offset + frConstArg}
  Mul -> UnkownWithConst{multiplier = multiplier * frConstArg, offset = offset * frConstArg}
  Sub -> case dir of
    ArgLike -> UnkownWithConst{multiplier = -1 * multiplier, offset = frConstArg - offset}
    Reversed -> UnkownWithConst{multiplier, offset = offset - frConstArg}
  Div -> case dir of
    ArgLike -> error "This Algorithm cannot work with unknown powered to anything else than 1. Attemted to create a negative power."
    Reversed -> UnkownWithConst{multiplier = multiplier / frConstArg, offset = offset / frConstArg}
 where
  frConstArg = floor constArg % 1

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

class FractionalOrIntegral a where
  divide :: a -> a -> a

instance FractionalOrIntegral Int where
  divide = div

instance FractionalOrIntegral Fraction where
  divide = (/)

evaluateOperation :: (FractionalOrIntegral a, Num a) => BinaryOperation -> a -> a -> a
evaluateOperation op = case op of
  Sum -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> divide

-- Types

newtype MonkeyId = MkMonkeyId String deriving (Eq, Hashable)

type Monkies = HashMap MonkeyId Monkey

data BinaryOperation = Sum | Sub | Mul | Div deriving (Show)

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
