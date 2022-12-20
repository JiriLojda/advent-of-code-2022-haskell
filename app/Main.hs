module Main (main) where

import System.IO

import Day1
import qualified Day2.Day2 as Day2
import qualified Day3.Solution as Day3
import qualified Day4.Solution as Day4
import qualified Day5.Solution as Day5
import qualified Day6.Solution as Day6
import qualified Day7.Solution as Day7
import qualified Day8.Solution as Day8
import qualified Day9.Solution as Day9
import qualified Day10.Solution as Day10
import qualified Day11.Solution as Day11
import qualified Day12.Solution as Day12
import qualified Day13.Solution as Day13
import qualified Day14.Solution as Day14

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
  _ -> pure "Not a valid day"

printDay :: IO String -> IO String -> IO String
printDay task1 task2 = do
  t1 <- fmap ("task1: " ++) task1
  t2<- fmap ("task2: " ++) task2
  pure (t1 ++ "\n" ++ t2)
