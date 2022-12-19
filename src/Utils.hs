module Utils
  ( loadFile
  , splitOn
  , head
  , tail
  , Parser
  , loadAndParseFile
  ) where

import System.IO
import Prelude hiding (head, tail)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Error as MP.Error
import Data.Void (Void)

loadFile :: String -> IO String
loadFile fileName = do
  handle <- openFile fileName ReadMode
  hGetContents handle

splitOn :: Char -> String -> (String, String)
splitOn char str = (takeWhile (/= char) str, (drop 1 $ dropWhile (/= char) str))

head :: [a] -> Maybe a
head [] = Nothing
head (x : _) = Just x

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_ : rest) = Just rest

type Parser = MP.Parsec Void String

loadAndParseFile :: String -> Parser a -> IO a
loadAndParseFile fileName parser = do
  contents <- Utils.loadFile fileName
  case MP.parse parser "" contents of
    Left err -> fail $ MP.Error.errorBundlePretty err
    Right res -> pure res
