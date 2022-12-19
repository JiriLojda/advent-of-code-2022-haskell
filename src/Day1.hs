module Day1 (day1Task1, day1Task2) where

import Data.List.Split
import Data.Function
import Data.List

day1Task1 :: Int
day1Task1 = result1 input

day1Task2 :: Int
day1Task2 = result2 input

result2 :: String -> Int
result2 str =
  str
    & parseBags
    & sort
    & reverse
    & take 3
    & sum

result1 :: String -> Int
result1 str = maximum $ parseBags str
  
parseBags :: String -> [Int]
parseBags str = map (sum . map parseInt . splitOn "\n") rawBags
  where
    rawBags = splitOn "\n\n" str

    parseInt :: String -> Int
    parseInt = read

input :: String
input = "1000\n\
\2000\n\
\3000\n\
\\n\
\4000\n\
\\n\
\5000\n\
\6000\n\
\\n\
\7000\n\
\8000\n\
\9000\n\
\\n\
\10000"
