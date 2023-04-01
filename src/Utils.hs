module Utils (
  loadFile,
  splitOn,
  head,
  tail,
  Parser,
  loadAndParseFile,
  orElse,
  last,
  parseString,
) where

import Data.Maybe qualified as Maybe
import Data.Void (Void)
import System.IO
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Error qualified as MP.Error
import Prelude hiding (head, last, tail)

loadFile :: String -> IO String
loadFile fileName = do
  handle <- openFile fileName ReadMode
  hGetContents handle

type Parser = MP.Parsec Void String

parseString :: Parser a -> String -> IO a
parseString parser input =
  case MP.parse parser "" input of
    Left err -> fail $ MP.Error.errorBundlePretty err
    Right res -> pure res

loadAndParseFile :: String -> Parser a -> IO a
loadAndParseFile fileName parser = do
  contents <- Utils.loadFile fileName
  parseString parser contents

splitOn :: Char -> String -> (String, String)
splitOn char str = (takeWhile (/= char) str, drop 1 $ dropWhile (/= char) str)

head :: [a] -> Maybe a
head [] = Nothing
head (x : _) = Just x

last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (_ : rest) = last rest

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_ : rest) = Just rest

orElse :: Maybe a -> a -> a
orElse = flip Maybe.fromMaybe
