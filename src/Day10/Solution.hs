{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Day10.Solution (task1, task2) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP.Char
import qualified Text.Megaparsec.Char.Lexer as MP.Char.L

import qualified Utils
import Utils (Parser)

data Op
  = Noop
  | AddX Int

data Cycle
  = OpInProgress
  | OpFinishing Op

data State = State
  { regX :: Int
  , cycle :: Int
  , drawnPixels :: String
  }

emptyState :: State
emptyState = State { regX = 1, cycle = 1, drawnPixels = "" }

task2 :: IO String
task2 = do
  ops <- Utils.loadAndParseFile "src/Day10/data.txt" opsParser
  let cycles = [cycle | op <- ops, cycle <- opToCycles op]
      result = reverse $ collectFromCycles collector [] cycles
  pure result
  where
    collector :: String -> (State, State) -> String
    collector collected (state, _)  | posOnScreenLine state.cycle `elem` [state.regX .. (state.regX + 2)] = '#' : addNewLineIfNeeded state.cycle collected
                                    | otherwise = '.' : addNewLineIfNeeded state.cycle collected
    
    posOnScreenLine :: Int -> Int
    posOnScreenLine cycle = ((cycle - 1) `mod` 40) + 1

    addNewLineIfNeeded :: Int -> String -> String
    addNewLineIfNeeded cycle input  | posOnScreenLine cycle == 1 = '\n' : input
                                    | otherwise = input

task1 :: IO Int
task1 = do
  ops <- Utils.loadAndParseFile "src/Day10/data.txt" opsParser
  let cycles = [cycle | op <- ops, cycle <- opToCycles op]
      result = sum $ collectFromCycles collector [] cycles
  pure result
  where
    collector :: [Int] -> (State, State) -> [Int]
    collector collected (_, state) | state.cycle `elem` [20, 60, 100, 140, 180, 220] = (state.cycle * state.regX) : collected
                              | otherwise = collected

collectFromCycles :: forall a. (a -> (State, State) -> a) -> a -> [Cycle] -> a
collectFromCycles collect initialAcc cycles = go initialAcc emptyState cycles
  where
    go :: a -> State -> [Cycle] -> a
    go !acc _ [] = acc
    go !acc state (cycle : restCycles) = go (collect acc (state, newState)) newState restCycles
      where
        newState = runCycle state cycle
        

runCycle :: State -> Cycle -> State
runCycle s OpInProgress = s { cycle = s.cycle + 1 }
runCycle s (OpFinishing op) = updatedState { cycle = updatedState.cycle + 1 }
  where
    updatedState = runOp s op

runOp :: State -> Op -> State
runOp s Noop = s
runOp s (AddX n) = s { regX = s.regX + n }

opToCycles :: Op -> [Cycle]
opToCycles Noop = [OpFinishing Noop]
opToCycles op@(AddX _) = [OpInProgress, OpFinishing op]

opsParser :: Parser [Op]
opsParser = MP.many opParser

opParser :: Parser Op
opParser = do
  res <- MP.choice
    [ MP.Char.string "noop" >> pure Noop
    , addXParser
    ]
  _ <- MP.Char.newline
  pure res
  where
    addXParser :: Parser Op
    addXParser = do
      _ <- MP.Char.string "addx"
      _ <- MP.Char.space
      num <- MP.Char.L.signed mempty MP.Char.L.decimal
      pure (AddX num)
