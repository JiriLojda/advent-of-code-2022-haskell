module Day20.Solution (
  task1,
  task2,
) where

import Data.Sequence (Seq)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP
import Text.Megaparsec.Char.Lexer qualified as MP.L

import Control.Monad qualified as Monad
import Data.Function ((&))
import Data.Sequence qualified as Seq
import Utils (Parser, loadAndParseFile)

task2 :: IO String
task2 = pure "Not Implemented"

task1 :: IO (Maybe Int)
task1 = do
  fileNums <- Seq.fromList <$> Utils.loadAndParseFile "src/Day20/data.txt" fileParser
  let mixed = performMixing fileNums
  pure $ do
    indexOf0 <- Seq.findIndexL (== 0) mixed
    coord1 <- Seq.lookup (wrapIndex mixed (indexOf0 + 1000)) mixed
    coord2 <- Seq.lookup (wrapIndex mixed (indexOf0 + 2000)) mixed
    coord3 <- Seq.lookup (wrapIndex mixed (indexOf0 + 3000)) mixed
    pure (coord1 + coord2 + coord3)

data FileEntry = MkFileEntry {index :: Int, value :: Int} deriving (Show)
data ProcessState = MkProcessState {sequencePos :: Int, originalSequencePos :: Int} deriving (Show)

performMixing :: Seq Int -> Seq Int
performMixing input =
  input
    & Seq.zipWith (\index value -> MkFileEntry{index, value}) (Seq.fromList $ take (Seq.length input) [0 ..])
    & process (MkProcessState{sequencePos = 0, originalSequencePos = 0})
    & fmap (.value)
 where
  process :: ProcessState -> Seq FileEntry -> Seq FileEntry
  process _ Seq.Empty = Seq.Empty
  process state currentSeq
    | state.sequencePos >= Seq.length currentSeq = currentSeq
    | Just entry <- getExpectedEntry state currentSeq = uncurry process (moveEntry entry state currentSeq)
    | otherwise = process (state{sequencePos = state.sequencePos + 1}) currentSeq

  getExpectedEntry :: ProcessState -> Seq FileEntry -> Maybe FileEntry
  getExpectedEntry state currentSeq = do
    entry <- Seq.lookup state.sequencePos currentSeq
    Monad.guard (entry.index == state.originalSequencePos)
    pure entry

  moveEntry :: FileEntry -> ProcessState -> Seq FileEntry -> (ProcessState, Seq FileEntry)
  moveEntry entry state currentSeq = (MkProcessState{sequencePos = newSeqPos, originalSequencePos = state.originalSequencePos + 1}, Seq.insertAt newIndex entry seqWithoutEntry)
   where
    seqWithoutEntry = Seq.deleteAt state.sequencePos currentSeq
    newIndex = calculateNewIndex entry state seqWithoutEntry
    newSeqPos = if newIndex <= state.sequencePos then state.sequencePos + 1 else state.sequencePos

  calculateNewIndex :: FileEntry -> ProcessState -> Seq FileEntry -> Int
  calculateNewIndex entry state currentSeq = wrapIndex currentSeq (state.sequencePos + entry.value)

wrapIndex :: Seq a -> Int -> Int
wrapIndex someSeq index
  | trimmedIndex < 0 = Seq.length someSeq - abs trimmedIndex - 1
  | otherwise = trimmedIndex
 where
  trimmedIndex = index `mod` Seq.length someSeq

fileParser :: Parser [Int]
fileParser = MP.L.signed mempty MP.L.decimal `MP.sepEndBy` MP.newline
