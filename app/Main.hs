module Main (main) where

import System.IO

import Day1
import Day10.Solution qualified as Day10
import Day11.Solution qualified as Day11
import Day12.Solution qualified as Day12
import Day13.Solution qualified as Day13
import Day14.Solution qualified as Day14
import Day15.Solution qualified as Day15
import Day16.Solution qualified as Day16
import Day17.Solution qualified as Day17
import Day18.Solution qualified as Day18
import Day19.Solution qualified as Day19
import Day2.Day2 qualified as Day2
import Day20.Solution qualified as Day20
import Day3.Solution qualified as Day3
import Day4.Solution qualified as Day4
import Day5.Solution qualified as Day5
import Day6.Solution qualified as Day6
import Day7.Solution qualified as Day7
import Day8.Solution qualified as Day8
import Day9.Solution qualified as Day9

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  print "What Day to try?"
  line <- getLine
  res <- callDay line
  putStrLn res

callDay :: String -> IO String
callDay str = case str of
  "1" -> printDay (pure $ show day1Task1) (pure $ show day1Task2)
  "2" -> printDay (fmap show Day2.task1) (fmap show Day2.task2)
  "3" -> printDay (fmap show Day3.task1) (fmap show Day3.task2)
  "4" -> printDay (fmap show Day4.task1) (fmap show Day4.task2)
  "5" -> printDay (fmap show Day5.task1) (fmap show Day5.task2)
  "6" -> printDay (fmap show Day6.task1) (fmap show Day6.task2)
  "7" -> printDay (fmap show Day7.task1) (fmap show Day7.task2)
  "8" -> printDay (fmap show Day8.task1) (fmap show Day8.task2)
  "9" -> printDay (fmap show Day9.task1) (fmap show Day9.task2)
  "10" -> printDay (fmap show Day10.task1) (fmap show Day10.task2)
  "11" -> printDay (fmap show Day11.task1) (fmap show Day11.task2)
  "12" -> printDay (fmap show Day12.task1) (fmap show Day12.task2)
  "13" -> printDay (fmap show Day13.task1) (fmap show Day13.task2)
  "14" -> printDay (fmap show Day14.task1) (fmap show Day14.task2)
  "15" -> printDay (fmap show Day15.task1) (fmap show Day15.task2)
  "16" -> printDay (fmap show Day16.task1) (fmap show Day16.task2)
  "17" -> printDay (fmap show Day17.task1) (fmap show Day17.task2)
  "18" -> printDay (fmap show Day18.task1) (fmap show (Day18.task2 Nothing))
  "19" -> printDay (fmap show Day19.task1) (fmap show Day19.task2)
  "20" -> printDay (fmap show Day20.task1) (fmap show Day20.task2)
  _ -> pure "Not a valid day"

printDay :: IO String -> IO String -> IO String
printDay task1 task2 = do
  t1 <- fmap ("task1: " ++) task1
  t2 <- fmap ("task2: " ++) task2
  pure (t1 ++ "\n" ++ t2)
