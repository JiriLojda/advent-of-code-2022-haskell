{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day22.Solution (
  task1,
  task2,
) where

import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import GHC.Generics (Generic)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP.Char
import Text.Megaparsec.Char.Lexer qualified as MP.Char.L
import Utils (Parser)
import Utils qualified
import Prelude hiding (Left, Right)

task2 :: IO String
task2 = pure "Not Implemented"

task1 :: IO Int
task1 = do
  (blockedPos, wraps, moves) <- Utils.loadAndParseFile "src/Day22/data.txt" dataParser
  print (show wraps)
  let initialState = MkMoveState{position = findStartingPosition wraps, direction = Right}
      finalState = moveOnMap blockedPos wraps moves initialState
  pure (calculateResult finalState)

calculateResult :: MoveState -> Int
calculateResult s = (1000 * snd adjustedPos) + (4 * fst adjustedPos) + directionValue
 where
  adjustedPos = (fst s.position + 1, snd s.position + 1)
  directionValue = case s.direction of
    Right -> 0
    Down -> 1
    Left -> 2
    Up -> 3

findStartingPosition :: Wraps -> Position
findStartingPosition wraps =
  wraps
    & Map.toList
    & filter ((== 0) . snd . snd)
    & map snd
    & List.sortOn fst
    & Utils.head
    & Maybe.fromMaybe (error "There is no starting position on this map.")

data MoveState = MkMoveState
  { position :: Position
  , direction :: Direction
  }

data Direction = Up | Down | Left | Right deriving (Show, Eq, Generic, Hashable)

moveOnMap :: BlockedPositions -> Wraps -> [Move] -> MoveState -> MoveState
moveOnMap _ _ [] s = s
moveOnMap blocked wraps (move : restMoves) state = case move of
  TurnRight -> moveOnMap blocked wraps restMoves state{direction = turnRight state.direction}
  TurnLeft -> moveOnMap blocked wraps restMoves state{direction = turnLeft state.direction}
  Forward 0 -> moveOnMap blocked wraps restMoves state
  Forward x ->
    if wrappedPosition `Set.member` blocked
      then moveOnMap blocked wraps restMoves state
      else moveOnMap blocked wraps (Forward (x - 1) : restMoves) state{position = wrappedPosition}
 where
  movedPosition = moveInDirection state.direction state.position
  wrappedPosition = Maybe.fromMaybe movedPosition $ Map.lookup (state.direction, movedPosition) wraps

moveInDirection :: Direction -> Position -> Position
moveInDirection direction position = case direction of
  Up -> (fst position, snd position - 1)
  Right -> (fst position + 1, snd position)
  Down -> (fst position, snd position + 1)
  Left -> (fst position - 1, snd position)

turnRight :: Direction -> Direction
turnRight Up = Right
turnRight Right = Down
turnRight Down = Left
turnRight Left = Up

turnLeft :: Direction -> Direction
turnLeft Up = Left
turnLeft Left = Down
turnLeft Down = Right
turnLeft Right = Up

type Position = (Int, Int) -- (x, y)

type BlockedPositions = HashSet Position
type Wraps = HashMap (Direction, Position) Position

data Tile
  = Space
  | Dot
  | Block
  | EndOfLine
  deriving (Eq, Show)

data Move
  = Forward Int
  | TurnRight
  | TurnLeft
  deriving (Show)

dataParser :: Parser (BlockedPositions, Wraps, [Move])
dataParser = do
  mapParserState <- mapParser emptyParserState
  moves <- movesParser
  pure (mapParserState.blockedRes, mapParserState.wrapsRes, moves)
 where
  emptyParserState :: ParserState
  emptyParserState =
    MkParserState
      { lastTile = Nothing
      , lastLine = []
      , currentLine = []
      , currentPos = (0, 0)
      , leftWrapPos = Nothing
      , topWrapPositions = []
      , blockedRes = mempty
      , wrapsRes = mempty
      }

movesParser :: Parser [Move]
movesParser = MP.many moveParser

moveParser :: Parser Move
moveParser =
  MP.choice
    [ Forward <$> MP.Char.L.decimal
    , MP.Char.char 'R' >> pure TurnRight
    , MP.Char.char 'L' >> pure TurnLeft
    ]

data ParserState = MkParserState
  { lastTile :: Maybe Tile
  , lastLine :: [Tile]
  , currentLine :: [Tile]
  , currentPos :: Position
  , leftWrapPos :: Maybe Int
  , topWrapPositions :: [Position]
  , blockedRes :: BlockedPositions
  , wrapsRes :: Wraps
  }

mapParser :: ParserState -> Parser ParserState
mapParser state = do
  tile <- tileParser
  let newState = moveParserState tile state
  if state.lastTile == Just EndOfLine && newState.lastTile == Just EndOfLine
    then pure newState
    else mapParser newState

type ParserStateModifier = (ParserState -> Tile -> Bool, ParserState -> ParserState)

moveParserState :: Tile -> ParserState -> ParserState
moveParserState tile state =
  [ movePosX
  , moveLine
  , setLastTile
  , addTile
  , addBlocks
  , addLeftXWrap
  , addHorizontalWrap
  , addTopYWrap
  , addVerticalWrapAboveSpace
  , addRestVerticalWraps
  ]
    & foldr applyModifier state
 where
  movePosX :: ParserStateModifier
  movePosX = (const (/= EndOfLine), \s -> s{currentPos = (fst s.currentPos + 1, snd s.currentPos)})

  setLastTile :: ParserStateModifier
  setLastTile = (const $ const True, \s -> s{lastTile = Just tile})

  addTile :: ParserStateModifier
  addTile = (const (/= EndOfLine), \s -> s{currentLine = tile : s.currentLine, lastLine = drop 1 s.lastLine})

  moveLine :: ParserStateModifier
  moveLine = (const (== EndOfLine), \s -> s{lastLine = reverse s.currentLine, currentLine = [], currentPos = (0, snd s.currentPos + 1)})

  addBlocks :: ParserStateModifier
  addBlocks = (const (== Block), \s -> s{blockedRes = Set.insert s.currentPos s.blockedRes})

  addLeftXWrap :: ParserStateModifier
  addLeftXWrap = (\s t -> (s.lastTile == Just Space || s.lastTile == Just EndOfLine) && isNonEmptyTile t, \s -> s{leftWrapPos = Just (fst s.currentPos)})

  addHorizontalWrap :: ParserStateModifier
  addHorizontalWrap = (\s t -> Maybe.isJust s.leftWrapPos && (fmap isNonEmptyTile s.lastTile == Just True) && t == EndOfLine, \s -> s{leftWrapPos = Nothing, wrapsRes = addWrap (leftRightWrap s) (rightLeftWrap s) s.wrapsRes})
   where
    leftRightWrap :: ParserState -> ((Direction, Position), Position)
    leftRightWrap s = ((Left, (Maybe.fromMaybe 0 s.leftWrapPos - 1, snd s.currentPos)), (fst s.currentPos - 1, snd s.currentPos))

    rightLeftWrap :: ParserState -> ((Direction, Position), Position)
    rightLeftWrap s = ((Right, s.currentPos), (Maybe.fromMaybe 0 s.leftWrapPos, snd s.currentPos))

  addTopYWrap :: ParserStateModifier
  addTopYWrap = (\s t -> isNonEmptyTile t && Maybe.fromMaybe Space (Utils.head s.lastLine) == Space, \s -> s{topWrapPositions = s.currentPos : s.topWrapPositions})

  addVerticalWrapAboveSpace :: ParserStateModifier
  addVerticalWrapAboveSpace =
    ( \s t -> t == Space && ((isNonEmptyTile <$> Utils.head s.lastLine) == Just True)
    , \s -> s{wrapsRes = addWrap (topDownWrap s) (bottomUpWrap s) s.wrapsRes}
    )
   where
    topWrap s = getTopWrap s.currentPos s

    topDownWrap :: ParserState -> ((Direction, Position), Position)
    topDownWrap s = ((Up, (fst (topWrap s), snd (topWrap s) - 1)), (fst s.currentPos, snd s.currentPos - 1))

    bottomUpWrap :: ParserState -> ((Direction, Position), Position)
    bottomUpWrap s = ((Down, s.currentPos), topWrap s)

  addRestVerticalWraps :: ParserStateModifier
  addRestVerticalWraps = (\s t -> t == EndOfLine && not (null s.lastLine), \s -> s{wrapsRes = List.foldl' (addRestWrap s) s.wrapsRes $ zip [0 ..] s.lastLine})
   where
    addRestWrap :: ParserState -> Wraps -> (Int, Tile) -> Wraps
    addRestWrap s wraps (index, t)
      | isNonEmptyTile t = addWrap topDownWrap bottomUpWrap wraps
      | otherwise = wraps
     where
      bottomWrapPos = (fst s.currentPos + index, snd s.currentPos - 1)
      topWrapPos = getTopWrap bottomWrapPos s

      topDownWrap = ((Up, (fst topWrapPos, snd topWrapPos - 1)), bottomWrapPos)
      bottomUpWrap = ((Down, (fst bottomWrapPos, snd bottomWrapPos + 1)), topWrapPos)

  getTopWrap :: Position -> ParserState -> Position
  getTopWrap pos s = Maybe.fromMaybe (fst pos, 0) (List.find ((== fst pos) . fst) s.topWrapPositions)

  isNonEmptyTile :: Tile -> Bool
  isNonEmptyTile t = t == Block || t == Dot

  addWrap :: ((Direction, Position), Position) -> ((Direction, Position), Position) -> Wraps -> Wraps
  addWrap entry1 entry2 wraps = uncurry Map.insert entry1 (uncurry Map.insert entry2 wraps)

  applyModifier :: ParserStateModifier -> ParserState -> ParserState
  applyModifier (shouldApply, apply) s = if shouldApply s tile then apply s else s

tileParser :: Parser Tile
tileParser =
  MP.choice
    [ MP.Char.char ' ' >> pure Space
    , MP.Char.char '.' >> pure Dot
    , MP.Char.char '#' >> pure Block
    , MP.Char.newline >> pure EndOfLine
    ]
