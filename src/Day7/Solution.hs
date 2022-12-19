{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day7.Solution (task1, task2) where

import qualified Data.List as List
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP.Char
import qualified Text.Megaparsec.Char.Lexer as MP.Char.L
import qualified Text.Megaparsec.Error as MP.Error
import Data.Void (Void)
import Data.Function ((&))
import qualified Data.Map as Map

import qualified Utils

data Command
  = Cd String
  | Ls
  deriving Show

data Input
  = Command Command
  | DirectoryInput String
  | FileInput Int String
  deriving Show

data FileSystem
  = Directory
    { name :: String
    , content :: [FileSystem]
    , size :: Int
    }
  | File { size :: Int, name :: String }
  deriving Show

foldFileSystem :: (a -> FileSystem -> a) -> a -> FileSystem -> a
foldFileSystem folder initialValue file@File {} = folder initialValue file
foldFileSystem folder initialValue dir@Directory {content} = flip folder dir $ foldl (foldFileSystem folder) initialValue content

task2 :: IO (Maybe Int)
task2 = do
  contents <- loadData
  case createFileSystem contents of
    Nothing -> fail "Failed to create file system."
    Just fs ->
      fs
        & foldFileSystem appendDir []
        & map (.size)
        & List.sort
        & dropWhile (< minSize fs)
        & Utils.head
        & pure
  where
    minSize :: FileSystem -> Int
    minSize fs = 30000000 - (70000000 - fs.size)

    appendDir :: [FileSystem] -> FileSystem -> [FileSystem]
    appendDir fss dir@Directory{} = dir : fss
    appendDir fss _ = fss

task1 :: IO Int
task1 = do
  contents <- loadData
  case createFileSystem contents of
    Nothing -> fail "Failed to create file system."
    Just fs -> pure $ foldFileSystem combineSize 0 fs
  where
    takeSize :: FileSystem -> Int
    takeSize File {} = 0
    takeSize fs = if fs.size <= 100000 then fs.size else 0

    combineSize :: Int -> FileSystem -> Int
    combineSize acc fs = acc + takeSize fs


createFileSystem :: [Input] -> Maybe FileSystem
createFileSystem [] = Nothing
createFileSystem (firstInput : restInputs) =
  case firstInput of
      Command (Cd "/") ->
        case go restInputs [] mempty of
          (content, []) -> Just (Directory { name = "/", content, size = sum $ map (.size) content })
          _ -> Nothing
      _ -> Nothing
  where
    go :: [Input] -> [(Int, String)] -> Map.Map String [FileSystem] -> ([FileSystem], [Input])
    go [] files dirs = (buildResult files dirs, [])
    go (i : rest) files dirs = case i of
      FileInput size name -> go rest ((size, name) : files) dirs
      DirectoryInput name -> go rest files (Map.insert name [] dirs)
      Command cmd -> case cmd of
        Ls -> go rest files dirs
        Cd ".." -> (buildResult files dirs, rest)
        Cd dirName ->
          let
            (contents, remainingInput) = go rest [] mempty
          in
          go remainingInput files (Map.insert dirName contents dirs)

    buildResult :: [(Int, String)] -> Map.Map String [FileSystem] -> [FileSystem]
    buildResult files dirs = map (uncurry File) files ++ map (uncurry createDir) (Map.assocs dirs)

    createDir :: String -> [FileSystem] -> FileSystem
    createDir name content = Directory { name, content, size = sum $ map (.size) content }


loadData :: IO [Input]
loadData = do
  contents <- Utils.loadFile "src/Day7/data.txt"
  case MP.parse dataParser "" contents of
    Left err -> fail $ MP.Error.errorBundlePretty err
    Right res -> pure res

type Parser = MP.Parsec Void String

dataParser :: Parser [Input]
dataParser = MP.many inputParser

inputParser :: Parser Input
inputParser =
  fmap Command commandParser
  <|> fmap DirectoryInput directoryParser
  <|> fmap (uncurry FileInput) fileParser
  where
    commandParser :: Parser Command
    commandParser = do
      _ <- MP.Char.char '$'
      _ <- MP.Char.space
      MP.choice
        [ do
            _ <- MP.Char.string "ls"
            _ <- MP.Char.newline
            pure Ls
        , do
            _ <- MP.Char.string "cd"
            _ <- MP.Char.space
            fmap Cd restOfLineParser
        ]
    
    directoryParser :: Parser String
    directoryParser = do
      _ <- MP.Char.string "dir"
      _ <- MP.Char.space
      restOfLineParser
    
    fileParser :: Parser (Int, String)
    fileParser = do
      size <- MP.Char.L.decimal
      _ <- MP.Char.space
      name <- restOfLineParser
      pure (size, name)

restOfLineParser :: Parser String
restOfLineParser = MP.manyTill MP.Char.printChar MP.Char.newline
