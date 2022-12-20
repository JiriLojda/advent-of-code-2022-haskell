module Day13.Solution (task1, task2) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP.Char
import qualified Text.Megaparsec.Char.Lexer as MP.Char.L
import Data.Function ((&))
import qualified Data.List as List

import qualified Utils
import Utils (Parser)

data PacketItem
  = Number Int
  | Lst [PacketItem]
  deriving (Show, Eq)

data Validity 
  = Valid
  | Invalid
  | Unknown
  deriving Eq

dividers :: [[PacketItem]]
dividers = [[Lst [Number 2]], [Lst [Number 6]]]

task2 :: IO Int
task2 = do
  originalPacketPairs <- Utils.loadAndParseFile "src/Day13/data.txt" packetPairsParser
  let packets = dividers ++ (originalPacketPairs >>= \(l, r) -> [l, r])
  packets
    & List.sortBy compareByValidity
    & zip [1..]
    & filter (isDividerPacket . snd)
    & map fst
    & product
    & pure
  where
    isDividerPacket :: [PacketItem] -> Bool
    isDividerPacket = flip List.elem dividers

compareByValidity :: [PacketItem] -> [PacketItem] -> Ordering
compareByValidity lst1 lst2 =
  case areListsCorrect lst1 lst2 of
    Valid -> LT
    Invalid -> GT
    Unknown -> EQ

task1 :: IO Int
task1 = do
  packetPairs <- Utils.loadAndParseFile "src/Day13/data.txt" packetPairsParser
  packetPairs
    & zip [1..]
    & filter ((== Valid) . (uncurry areListsCorrect) . snd)
    & map fst
    & sum
    & pure

fallback :: Validity -> Validity -> Validity
fallback Unknown v = v
fallback v _ = v

areCorrect :: PacketItem -> PacketItem -> Validity
areCorrect (Number l) (Number r)  | l < r = Valid
                                  | l > r = Invalid
                                  | otherwise = Unknown
areCorrect num@(Number _) (Lst lst) = areListsCorrect [num] lst
areCorrect (Lst lst) num@(Number _) = areListsCorrect lst [num]
areCorrect (Lst lstL) (Lst lstR) = areListsCorrect lstL lstR

areListsCorrect :: [PacketItem] -> [PacketItem] -> Validity
areListsCorrect [] [] = Unknown
areListsCorrect [] _ = Valid
areListsCorrect _ [] = Invalid
areListsCorrect (l : restL) (r : restR) =
  areCorrect l r `fallback` areListsCorrect restL restR

packetPairsParser :: Parser [([PacketItem], [PacketItem])]
packetPairsParser = packetPairParser `MP.sepEndBy` MP.Char.newline

packetPairParser :: Parser ([PacketItem], [PacketItem])
packetPairParser = do
  firstP <- listPacketParser
  _ <- MP.Char.newline
  secondP <- listPacketParser
  _ <- MP.Char.newline
  case (firstP, secondP) of
    (Lst f, Lst s) -> pure (f, s)
    _ -> fail "Top-level packets are not lists, but should be."

packetParser :: Parser PacketItem
packetParser = MP.choice [fmap Number MP.Char.L.decimal, listPacketParser]

listPacketParser :: Parser PacketItem
listPacketParser = do
  _ <- MP.Char.char '['
  items <- packetParser `MP.sepBy` MP.Char.char ','
  _ <- MP.Char.char ']'
  pure $ Lst items
