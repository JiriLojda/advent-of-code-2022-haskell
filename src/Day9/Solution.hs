{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day9.Solution (task1, task2) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP.Char
import qualified Text.Megaparsec.Char.Lexer as MP.Char.L
import qualified Data.Set as Set
import Prelude hiding (Right, Left)

import qualified Utils
import Utils (Parser)

data Move
  = Up Int
  | Down Int
  | Left Int
  | Right Int

type Position = (Int, Int)
newtype HeadPos = HeadPos Position
newtype TailPos = TailPos Position deriving newtype (Ord, Eq)
type LongRope = [(HeadPos, TailPos)]

task2 :: IO Int
task2 = do
  moves <- Utils.loadAndParseFile "src/Day9/data.txt" movesParser
  let rope = take 9 $ repeat (HeadPos (0, 0), TailPos (0, 0))
  let tailPositions = foldRopeMoves (\set r -> Set.insert (snd . lastOrThrow $ r) set) performSingleLongRopeMove (Set.singleton $ TailPos (0, 0)) rope moves
  pure $ Set.size tailPositions
  where
    lastOrThrow :: [a] -> a
    lastOrThrow [] = error "Cannot take the last element of an empty list."
    lastOrThrow [x] = x
    lastOrThrow (_ : rest) = lastOrThrow rest

performSingleLongRopeMove :: LongRope -> Move -> (LongRope, Move)
performSingleLongRopeMove [] _ = error "Invalid rope, LongRope cannot be empty"
performSingleLongRopeMove (firstPart : restParts) move = (newFirstPart : go (tailToHead . snd $ newFirstPart) restParts [], newMove)
  where
    (newFirstPart, newMove) = performSingleMove firstPart move

    tailToHead :: TailPos -> HeadPos
    tailToHead (TailPos pos) = HeadPos pos

    go :: HeadPos -> LongRope -> LongRope -> LongRope
    go _ [] acc = reverse acc
    go newHeadPos ((_, oldTailPos) : rest) acc = go (tailToHead newTailPos) rest ((newHeadPos, newTailPos) : acc)
      where
        newTailPos = moveTail newHeadPos oldTailPos

task1 :: IO Int
task1 = do
  moves <- Utils.loadAndParseFile "src/Day9/data.txt" movesParser
  let rope = (HeadPos (0, 0), TailPos (0, 0))
  let tailPositions = foldRopeMoves (\set (_, pos) -> Set.insert pos set) performSingleMove (Set.singleton $ snd rope) rope moves
  pure $ Set.size tailPositions

foldRopeMoves :: forall acc r. (acc -> r -> acc) -> (r -> Move -> (r, Move)) -> acc -> r -> [Move] -> acc
foldRopeMoves folder performMove = go
  where
    go :: acc -> r -> [Move] -> acc
    go res _ [] = res
    go res rope (move : restMoves) =
      if moveSize newMove == 0
        then go newRes newRope restMoves
        else go newRes newRope (newMove : restMoves)
      where
        (newRope, newMove) = performMove rope move
        newRes = folder res newRope

moveSize :: Move -> Int
moveSize (Up n) = n
moveSize (Down n) = n
moveSize (Left n) = n
moveSize (Right n) = n

performSingleMove :: (HeadPos, TailPos) -> Move -> ((HeadPos, TailPos), Move)
performSingleMove (HeadPos headPos, tailPos) move = ((HeadPos newHeadPos, newTailPos), newMove)
  where
    (newMove, movePos) = moveByOne move
    newHeadPos = sumPositions movePos headPos
    newTailPos = moveTail (HeadPos newHeadPos) tailPos

moveTail :: HeadPos -> TailPos -> TailPos
moveTail (HeadPos headPos) (TailPos tailPos) =
  if abs distX < 2 && abs distY < 2
    then TailPos tailPos
    else TailPos (fst tailPos + signum distX, snd tailPos + signum distY)
  where
    (distX, distY) = diffPositions headPos tailPos

moveByOne :: Move -> (Move, Position)
moveByOne (Up n)    = (Up (n - 1), (0, -1))
moveByOne (Down n)  = (Down (n - 1), (0, 1))
moveByOne (Left n)  = (Left (n - 1), (-1, 0))
moveByOne (Right n) = (Right (n - 1), (1, 0))

sumPositions :: Position -> Position -> Position
sumPositions (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

diffPositions :: Position -> Position -> Position
diffPositions (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

movesParser :: Parser [Move]
movesParser = MP.many moveParser

moveParser :: Parser Move
moveParser = do
  moveType <- MP.choice
    [ MP.Char.char 'U'
    , MP.Char.char 'D'
    , MP.Char.char 'L'
    , MP.Char.char 'R'
    ]
  MP.Char.space
  num <- MP.Char.L.decimal
  _ <- MP.Char.newline
  case moveType of
    'U' -> pure (Up num)
    'D' -> pure (Down num)
    'L' -> pure (Left num)
    'R' -> pure (Right num)
    _ -> fail "Invariant violated. Received unexpected char."
