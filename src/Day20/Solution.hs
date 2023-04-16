module Day20.Solution (
  task1,
  task2,
) where

import Data.Sequence (Seq)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP
import Text.Megaparsec.Char.Lexer qualified as MP.L

import Control.Monad qualified as Monad
import Data.List qualified as List
import Data.Sequence qualified as Seq
import Utils (Parser, loadAndParseFile)

decryptionKey :: Int
decryptionKey = 811589153

task2 :: IO (Maybe Int)
task2 = do
  fileNums <- Seq.fromList <$> Utils.loadAndParseFile "src/Day20/data.txt" fileParser
  let inputWithDecryptionKey = fmap (* decryptionKey) fileNums
  let mixed = List.foldl' (\currentSeq _ -> performMixing getNextIndexForMultiPass currentSeq) (convertToFileEntries inputWithDecryptionKey) [1 .. 10 :: Int]
  pure (calculateResult $ convertFromFileEntries mixed)

task1 :: IO (Maybe Int)
task1 = do
  fileNums <- Seq.fromList <$> Utils.loadAndParseFile "src/Day20/data.txt" fileParser
  let mixed = performMixing getNextIndexForSinglePass $ convertToFileEntries fileNums
  pure (calculateResult $ convertFromFileEntries mixed)

calculateResult :: Seq Int -> Maybe Int
calculateResult items = do
  indexOf0 <- Seq.findIndexL (== 0) items
  coord1 <- Seq.lookup (wrapIndex items (indexOf0 + 1000)) items
  coord2 <- Seq.lookup (wrapIndex items (indexOf0 + 2000)) items
  coord3 <- Seq.lookup (wrapIndex items (indexOf0 + 3000)) items
  pure (coord1 + coord2 + coord3)

data FileEntry = MkFileEntry {index :: Int, value :: Int} deriving (Show)

convertToFileEntries :: Seq Int -> Seq FileEntry
convertToFileEntries input = Seq.zipWith (\index value -> MkFileEntry{index, value}) (Seq.fromList $ take (Seq.length input) [0 ..]) input

convertFromFileEntries :: Seq FileEntry -> Seq Int
convertFromFileEntries = fmap (.value)

data ProcessState = MkProcessState {sequencePos :: Int, originalSequencePos :: Int} deriving (Show)

data NextIndexContext = MkNextIndexContext {currentIndex :: Int, nextIndexInOriginalSeq :: Int, newEntryIndex :: Int, newSeq :: Seq FileEntry}

-- for task one leading to O(n * log n) complexity (can only handle 1 mixing pass)
getNextIndexForSinglePass :: NextIndexContext -> Maybe Int
getNextIndexForSinglePass ctx
  | ctx.newEntryIndex <= ctx.currentIndex && ctx.currentIndex >= Seq.length ctx.newSeq - 1 = Nothing
  | ctx.newEntryIndex <= ctx.currentIndex = Just (ctx.currentIndex + 1)
  | otherwise = Just ctx.currentIndex

-- for task two leasing to O(n * n * log n) complexity (handles multiple mixing passes)
getNextIndexForMultiPass :: NextIndexContext -> Maybe Int
getNextIndexForMultiPass ctx = Seq.findIndexL ((== ctx.nextIndexInOriginalSeq) . (.index)) ctx.newSeq

performMixing :: (NextIndexContext -> Maybe Int) -> Seq FileEntry -> Seq FileEntry
performMixing getNextIndex = process (MkProcessState{sequencePos = 0, originalSequencePos = 0})
 where
  process :: ProcessState -> Seq FileEntry -> Seq FileEntry
  process _ Seq.Empty = Seq.Empty
  process state currentSeq
    | state.sequencePos >= Seq.length currentSeq = currentSeq
    | Just entry <- getExpectedEntry state currentSeq = case moveEntry entry state currentSeq of
        (Nothing, finalSeq) -> finalSeq
        (Just nextState, nextSeq) -> process nextState nextSeq
    | otherwise = process (state{sequencePos = state.sequencePos + 1}) currentSeq

  getExpectedEntry :: ProcessState -> Seq FileEntry -> Maybe FileEntry
  getExpectedEntry state currentSeq = do
    entry <- Seq.lookup state.sequencePos currentSeq
    Monad.guard (entry.index == state.originalSequencePos)
    pure entry

  moveEntry :: FileEntry -> ProcessState -> Seq FileEntry -> (Maybe ProcessState, Seq FileEntry)
  moveEntry entry state currentSeq = (newState, finalSeq)
   where
    seqWithoutEntry = Seq.deleteAt state.sequencePos currentSeq
    finalSeq = Seq.insertAt newIndex entry seqWithoutEntry
    newIndex = calculateNewIndex entry state seqWithoutEntry
    newSeqPos = getNextIndex MkNextIndexContext{currentIndex = state.sequencePos, nextIndexInOriginalSeq = state.originalSequencePos + 1, newEntryIndex = newIndex, newSeq = finalSeq}
    newState = case newSeqPos of
      Nothing -> Nothing
      Just sequencePos -> Just MkProcessState{sequencePos, originalSequencePos = state.originalSequencePos + 1}

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
