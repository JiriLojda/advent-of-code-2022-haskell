{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day2.Day2 (task1, task2) where

import System.IO
import Data.Maybe
import Prelude hiding (round)

data GameChoice
  = Rock
  | Paper
  | Scissors
  deriving (Show, Eq)

data Round = MkRound
  { myChoice :: GameChoice
  , opponentChoice :: GameChoice
  } deriving Show


task1 :: IO Int
task1 = fmap (sum . map roundResult) loadData


roundResult :: Round -> Int
roundResult round | round.myChoice == round.opponentChoice = 3 + choiceScore round.myChoice
                  | otherwise = if isWin round then 6 + choiceScore round.myChoice else 0 + choiceScore round.myChoice
  where
    choiceScore :: GameChoice -> Int
    choiceScore Rock = 1
    choiceScore Paper = 2
    choiceScore Scissors = 3

    isWin :: Round -> Bool
    isWin MkRound { myChoice = Rock, opponentChoice = Scissors } = True
    isWin MkRound { myChoice = Paper, opponentChoice = Rock } = True
    isWin MkRound { myChoice = Scissors, opponentChoice = Paper } = True
    isWin MkRound { myChoice = Paper, opponentChoice = Scissors } = False
    isWin MkRound { myChoice = Rock, opponentChoice = Paper } = False
    isWin MkRound { myChoice = Scissors, opponentChoice = Rock } = False
    isWin _ = False


task2 :: IO Int
task2 = fmap (sum . map roundResult . map adjustRound) loadData
  where
    adjustRound :: Round -> Round
    adjustRound round = round { myChoice = myNewChoice round }

    myNewChoice :: Round -> GameChoice
    myNewChoice MkRound { myChoice = Rock, opponentChoice = Rock } = Scissors
    myNewChoice MkRound { myChoice = Rock, opponentChoice = Paper } = Rock
    myNewChoice MkRound { myChoice = Rock, opponentChoice = Scissors } = Paper
    myNewChoice MkRound { myChoice = Paper, opponentChoice = Rock } = Rock
    myNewChoice MkRound { myChoice = Paper, opponentChoice = Paper } = Paper
    myNewChoice MkRound { myChoice = Paper, opponentChoice = Scissors } = Scissors
    myNewChoice MkRound { myChoice = Scissors, opponentChoice = Rock } = Paper
    myNewChoice MkRound { myChoice = Scissors, opponentChoice = Paper } = Scissors
    myNewChoice MkRound { myChoice = Scissors, opponentChoice = Scissors } = Rock


loadData :: IO [Round]
loadData = do
  handle <- openFile "src/Day2/data.txt" ReadMode
  contents <- hGetContents handle
  pure $ catMaybes $ map parseRound $ lines contents
  where
    parseRound :: String -> Maybe Round
    parseRound (opponent : ' ' : my : []) = MkRound <$> parseMyChoice my <*> parseOpponentChoice opponent
    parseRound _                            = Nothing

    parseOpponentChoice :: Char -> Maybe GameChoice
    parseOpponentChoice 'A' = Just Rock
    parseOpponentChoice 'B' = Just Paper
    parseOpponentChoice 'C' = Just Scissors
    parseOpponentChoice _   = Nothing

    parseMyChoice :: Char -> Maybe GameChoice
    parseMyChoice 'Y' = Just Paper
    parseMyChoice 'X' = Just Rock
    parseMyChoice 'Z' = Just Scissors
    parseMyChoice _   = Nothing
