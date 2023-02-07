{-# LANGUAGE StrictData #-}

module Day17.Solution (task1, task2) where

import Prelude hiding (Either(..))
import qualified Prelude
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP.Char
import qualified Data.Maybe as Maybe
import qualified Data.HashSet as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq ((:|>), (:<|)))
import qualified Data.Foldable as Foldable
import qualified Data.Function as Fnc
import Data.Function ((&))

import Debug.Trace

import qualified Utils
import Utils (Parser)
import qualified Day17.CyclicList as CyclicList
import Day17.CyclicList (CyclicList)

printXRows :: Int -> State -> String
printXRows 0 _ = "|-------|\n"
printXRows numLines state =
  ('|' : map (\x -> printPos (x, numLines - 1)) [0..6] ++ "|\n") ++ printXRows (numLines - 1) state
  where
    fallingRockTiles = map (addPos state.rockPosition) (rockPositions state.rockType)

    printPos :: Position -> Char
    printPos pos
      | pos `Set.member` state.blockedTiles = '#'
      | pos `elem` fallingRockTiles = '@'
      | otherwise = '.'

spawnRockMargins :: (Int, Int)
spawnRockMargins = (2, 3)

task2 :: IO Int
task2 = do
  rocks <- Utils.loadAndParseFile "src/Day17/rocks.txt" rocksParser
  moves <- Utils.loadAndParseFile "src/Day17/sampleData.txt" movesParser

  let (initialRocks, initialState) = createInitialState rocks
      finalState = fallXRocks 10000000 envProps initialRocks moves initialState
  pure (snd finalState.highestPoint + 1)
  where
    envProps = MkEnvProps
      { fallSize = 1
      , streamPushSize = 1
      , roomWidth = 7
      , maxKeptBlockedPos = 100
      }

task1 :: IO Int
task1 = do
  rocks <- Utils.loadAndParseFile "src/Day17/rocks.txt" rocksParser
  moves <- Utils.loadAndParseFile "src/Day17/data.txt" movesParser

  let (initialRocks, initialState) = createInitialState rocks
      finalState = fallXRocks 2022 envProps initialRocks moves initialState
  -- putStrLn $ printXRows 21 finalState
  pure (snd finalState.highestPoint + 1)
  where
    envProps = MkEnvProps
      { fallSize = 1
      , streamPushSize = 1
      , roomWidth = 7
      , maxKeptBlockedPos = 10000
      }

createInitialState :: CyclicList Rock -> (CyclicList Rock, State)
createInitialState initialRocks = spawnRock spawnRockMargins initialRocks $ MkState
  { blockedTiles = mempty
  , highestPoint = (0, -1)
  , rockType = MkRock []
  , rockPosition = (0, 0)
  , recentBlockedPos = mempty
  }

fallXRocks :: Int -> EnvProps -> CyclicList Rock -> CyclicList StreamMove -> State -> State
fallXRocks numRocks envProps rocks moves initialState = res
  where
    (_, _, res) = Foldable.foldl' (\x n -> fallNextRock (if n `mod` 1000000 == 0 then trace ("rock #" ++ show n) x else x)) (rocks, moves, initialState) [(1 :: Int)..numRocks]

    fallNextRock :: (CyclicList Rock, CyclicList StreamMove, State) -> (CyclicList Rock, CyclicList StreamMove, State)
    fallNextRock (!rs, !ms, !state) = (newRocks, newMoves, newState)
      where
        (!newMoves, !stateAfterFall) = makeRockFallUntilRest envProps ms state
        (!newRocks, !newState) = spawnRock spawnRockMargins rs stateAfterFall

data StreamMove
  = Left
  | Right
  deriving Show

type Position = (Int, Int)

newtype Rock = MkRock [Position] deriving Show -- Positions from the bottom left corner of the rock profile

rockPositions :: Rock -> [Position]
rockPositions (MkRock pos) = pos

data State = MkState
  { blockedTiles :: Set.HashSet Position
  , highestPoint :: Position
  , rockType :: Rock
  , rockPosition :: Position
  , recentBlockedPos :: Seq.Seq Position
  } deriving Show

data MoveType
  = Fall
  | StremPush
  deriving Show

data EnvProps = MkEnvProps
  { fallSize :: Int
  , streamPushSize :: Int
  , roomWidth :: Int
  , maxKeptBlockedPos :: Int -- performance optimization not to grow the set too big (after certain amount of position the old positions become irrelevant)
  } deriving Show

spawnRock :: (Int, Int) -> CyclicList Rock -> State -> (CyclicList Rock, State)
spawnRock (fromLeft, fromHighestRock) rocks state =
  (newRocks, state
    { rockType = newRock
    , rockPosition = (fromLeft, snd state.highestPoint + fromHighestRock + 1) 
    })
  where
    (newRock, newRocks) = CyclicList.readItem rocks

makeRockFallUntilRest :: EnvProps -> CyclicList StreamMove -> State -> (CyclicList StreamMove, State)
makeRockFallUntilRest envProps = go
  where
    go :: CyclicList StreamMove -> State -> (CyclicList StreamMove, State)
    go moves state = if didMoveDown state.rockPosition nextState.rockPosition
      then go nextMoves nextState
      else (nextMoves, nextState)
      where
        (streamMove, nextMoves) = CyclicList.readItem moves
        nextState = makeMove envProps streamMove state

makeMove :: EnvProps -> StreamMove -> State -> State
makeMove envProps streamMove state =
  state
    & pushWithStream envProps streamMove
    & makeRockFall envProps
    & (\newS -> if didMoveDown state.rockPosition newS.rockPosition then newS else restRock envProps newS)

didMoveDown :: Position -> Position -> Bool
didMoveDown (_, prevY) (_, nextY) = nextY < prevY

restRock :: EnvProps -> State -> State
restRock envProps state =
  state
    { blockedTiles = newBlockedSet
    , highestPoint = Foldable.maximumBy (compare `Fnc.on` snd) (state.highestPoint : newBlocked)
    , recentBlockedPos = newRecentBlocked
    }
  where
    (newRecentBlocked, newBlockedSet) = Foldable.foldl' (addBlocked envProps.maxKeptBlockedPos) (state.recentBlockedPos, state.blockedTiles) newBlocked

    newBlocked = map (addPos state.rockPosition) (rockPositions state.rockType)

    addBlocked :: Int -> (Seq.Seq Position, Set.HashSet Position) -> Position -> (Seq.Seq Position, Set.HashSet Position)
    addBlocked maxSetSize inputs newBlockedPos = (newBlockedPos :<| recentBlocked, Set.insert newBlockedPos blocked)
      where
        (recentBlocked, blocked) = if Seq.length (fst inputs) >= maxSetSize
          then removeLast inputs
          else inputs
    
    removeLast :: (Seq.Seq Position, Set.HashSet Position) -> (Seq.Seq Position, Set.HashSet Position)
    removeLast (Seq.Empty, s) = (Seq.Empty, s)
    removeLast (prev :|> lastPos, s) = (prev, Set.delete lastPos s)

makeRockFall :: EnvProps -> State -> State
makeRockFall envProps state =
  if isValidPosition envProps state.blockedTiles state.rockType newRockPos
    then state { rockPosition = newRockPos }
    else state
  where
    newRockPos = moveDownBy envProps.fallSize state.rockPosition

pushWithStream :: EnvProps -> StreamMove -> State -> State
pushWithStream envProps streamMove state =
  if isValidPosition envProps state.blockedTiles state.rockType newRockPos
    then state { rockPosition = newRockPos }
    else state
  where
    moveFnc = case streamMove of
      Left -> moveLeftBy
      Right -> moveRightBy
    
    newRockPos = moveFnc envProps.streamPushSize state.rockPosition

isValidPosition :: EnvProps -> Set.HashSet Position -> Rock -> Position -> Bool
isValidPosition envProps blocked rock bottomLeftPos = all valid (rockPositions rock)
  -- rockPositions rock
    -- & map (addPos bottomLeftPos)
    -- & all (\pos -> doesntBreakBottom pos && doesntBreakLeftWall pos && doesntBreakRightWall pos && doesntCollideWithBlocked pos)
  where
    valid :: Position -> Bool
    valid relPos = doesntBreakBottom absPos && doesntBreakLeftWall absPos && doesntBreakRightWall absPos && doesntCollideWithBlocked absPos
      where
        !absPos = addPos bottomLeftPos relPos
    -- also :: (a -> Bool) -> (a -> Bool) -> a -> Bool
    -- also !predicate1 !predicate2 !v = predicate1 v && predicate2 v
    
    -- myPred :: Position -> Bool
    -- myPred !x = doesntCollideWithBlocked x `a` doesntBreakBottom x `a` doesntBreakLeftWall x `a` doesntBreakRightWall x
    
    -- a :: Bool -> Bool -> Bool
    -- a !x !y = x && y

    doesntCollideWithBlocked :: Position -> Bool 
    doesntCollideWithBlocked = not . flip Set.member blocked

    doesntBreakRightWall :: Position -> Bool
    doesntBreakRightWall = (< envProps.roomWidth) . fst

    doesntBreakLeftWall :: Position -> Bool
    doesntBreakLeftWall = (>= 0) . fst

    doesntBreakBottom :: Position -> Bool
    doesntBreakBottom = (>= 0) . snd

moveRightBy :: Int -> Position -> Position
moveRightBy diff = moveBy (diff, 0)

moveLeftBy :: Int -> Position -> Position
moveLeftBy diff = moveBy (-1 * diff, 0)

moveDownBy :: Int -> Position -> Position
moveDownBy diff = moveBy (0, -1 * diff)

moveBy :: (Int, Int) -> Position -> Position
moveBy = addPos

addPos :: Position -> Position -> Position
addPos (!x1, !y1) (x2, y2) = (x, y)
  where
    !x = x1 + x2
    !y = y1 + y2

-- Parsers
rocksParser :: Parser (CyclicList Rock)
rocksParser = do
  rocks <- rockParser `MP.sepEndBy` MP.Char.newline
  case CyclicList.create rocks of
    Prelude.Left err -> fail err
    Prelude.Right res -> pure res

rockSymbol :: Char
rockSymbol = '#'

rockParser :: Parser Rock
rockParser = do
  rockLines <- MP.some rockCharParser `MP.sepEndBy` MP.Char.newline
  pure (createRock rockLines)
  where
    rockCharParser :: Parser Char
    rockCharParser = MP.choice
      [ MP.Char.char '.'
      , MP.Char.char rockSymbol
      ]

createRock :: [[Char]] -> Rock
createRock chars = MkRock (go 0 (reverse chars) [])
  where
    go :: Int -> [[Char]] -> [Position] -> [Position]
    go _ [] acc = acc
    go !lineNum (line : restLines) acc = go (lineNum + 1) restLines (lineToPositions lineNum line ++ acc)

    lineToPositions :: Int -> [Char] -> [Position]
    lineToPositions lineNum line = Maybe.mapMaybe (\(k, v) -> if v == rockSymbol then Just (k, lineNum) else Nothing) (zip [0..] line)

movesParser :: Parser (CyclicList StreamMove)
movesParser = do
  moves <- MP.some moveParser
  case CyclicList.create moves of
    Prelude.Left err -> fail err
    Prelude.Right res -> pure res

moveParser :: Parser StreamMove
moveParser = MP.choice
  [ MP.Char.char '>' >> pure Right
  , MP.Char.char '<' >> pure Left
  ]
