{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Day17.Solution (task1, task2) where

import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Function qualified as Fnc
import Data.HashSet qualified as Set
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP.Char
import Prelude hiding (Either (..))
import Prelude qualified

import Debug.Trace

import Data.Bifunctor qualified as BiF
import Day17.CyclicList (CyclicList)
import Day17.CyclicList qualified as CyclicList
import Utils (Parser)
import Utils qualified

printXRows :: Int -> State -> String
printXRows 0 _ = "|-------|\n"
printXRows numLines state =
  ('|' : map (\x -> printPos (x, numLines - 1)) [0 .. 6] ++ "|\n") ++ printXRows (numLines - 1) state
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
  -- pure 99

  rocks <- Utils.loadAndParseFile "src/Day17/rocks.txt" rocksParser
  moves <- Utils.loadAndParseFile "src/Day17/data.txt" movesParser

  let (initialRocks, initialState) = createInitialState rocks
      rocksNumber = 1000000000000
      (finalState, maybeRepetitionSize) = fallUntilRepetitionOrMaxRocks rocksNumber envProps initialRocks moves initialState

  case maybeRepetitionSize of
    Nothing -> pure (snd finalState.highestPoint + 1)
    Just repetitionSize -> pure (calculateTotalHeight rocksNumber finalState repetitionSize)
 where
  envProps =
    MkEnvProps
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
      rocksNumber = 2022
      (finalState, maybeRepetitionSize) = fallUntilRepetitionOrMaxRocks rocksNumber envProps initialRocks moves initialState

  case maybeRepetitionSize of
    Nothing -> pure (snd finalState.highestPoint + 1)
    Just repetitionSize -> pure (calculateTotalHeight rocksNumber finalState repetitionSize)
 where
  envProps =
    MkEnvProps
      { fallSize = 1
      , streamPushSize = 1
      , roomWidth = 7
      , maxKeptBlockedPos = 10000
      }

createInitialState :: CyclicList Rock -> (CyclicList Rock, State)
createInitialState initialRocks =
  spawnRock spawnRockMargins initialRocks $
    MkState
      { blockedTiles = mempty
      , highestPoint = (0, -1)
      , rockType = MkRock []
      , rockPosition = (0, 0)
      , recentBlockedPos = mempty
      , rocksInTower = []
      }

fallUntilRepetitionOrMaxRocks :: Int -> EnvProps -> CyclicList Rock -> CyclicList StreamMove -> State -> (State, Maybe Int) -- (resulting state, num of rocks in a repetition window)
fallUntilRepetitionOrMaxRocks numRocks envProps rocks moves initialState = (resState, resChunkSize)
 where
  (_, _, resState, resChunkSize) = go numRocks rocks moves initialState Nothing
  movesLength = CyclicList.length moves

  go :: Int -> CyclicList Rock -> CyclicList StreamMove -> State -> Maybe Int -> (CyclicList Rock, CyclicList StreamMove, State, Maybe Int)
  go _ rs ms s (Just x) = (rs, ms, s, Just x)
  go 0 rs ms s Nothing = (rs, ms, s, Nothing)
  go !rockNum !rs !ms !state Nothing = go (rockNum - 1) newRocks newMoves newState maybeChunkSize
   where
    (!newMoves, !stateAfterFall) = makeRockFallUntilRest envProps ms $ if True then trace ("rock #" ++ show (numRocks - rockNum)) state else state
    (!newRocks, !newState) = spawnRock spawnRockMargins rs stateAfterFall
    maybeChunkSize = createRepetitionChunkSize newState

  createRepetitionChunkSize :: State -> Maybe Int
  createRepetitionChunkSize s = findRepetitionIndex (take movesLength rocksX) rocksX
   where
    rocksX = map (.x) s.rocksInTower
  -- processWindows old = if List.isInfixOf (take 40 old.currentWindow) (drop 40 old.currentWindow) then trace ("Found!!!" ++ show (length old.currentWindow) ++ " matching chunks: " ++ show (findMatchingChunks (take 40 old.currentWindow) (drop 40 old.currentWindow))) old else old

  findRepetitionIndex :: (Eq a) => [a] -> [a] -> Maybe Int
  findRepetitionIndex subList list =
    case matchingChunks of
      (firstMatch : _ : _) ->
        let
          firstIndex = movesLength + firstMatch
         in
          if take firstIndex list == take firstIndex (drop firstIndex list) then Just firstIndex else Nothing
      _ -> Nothing
   where
    matchingChunks = findMatchingChunks subList (drop movesLength list)

  findMatchingChunks :: forall a. (Eq a) => [a] -> [a] -> [Int]
  findMatchingChunks sublist = chunksGo 0
   where
    subListLen = length sublist

    chunksGo :: Int -> [a] -> [Int]
    chunksGo _ [] = []
    chunksGo index list
      | chunk == sublist = index : chunksGo (index + subListLen) restList
      | otherwise = chunksGo (index + subListLen) restList
     where
      (chunk, restList) = splitAt subListLen list

calculateTotalHeight ::
  Int -> -- total number of rocks to fall
  State -> -- current state
  Int -> -- found repeating chunk size
  Int -- total tower height
calculateTotalHeight totalRocksNum state chunkSize = snd state.highestPoint + 1 + rocksAfterLastChunkHeight + (chunksNum * chunkHeight)
 where
  rocksNumBeforeFirstChunk = length state.rocksInTower
  chunksNum = countChunksThatFit totalRocksNum chunkSize rocksNumBeforeFirstChunk
  rocksNumAfterLastChunk = totalRocksNum - (chunksNum * chunkSize) - rocksNumBeforeFirstChunk
  beforeChunkTotalHeight = maybe 0 (.towerHeightWithoutThis) $ Utils.last (take chunkSize state.rocksInTower)
  afterRestRocksTotalHeight = maybe 0 (.towerheightWithThis) $ Utils.head (take rocksNumAfterLastChunk $ drop (chunkSize - rocksNumAfterLastChunk) state.rocksInTower)
  rocksAfterLastChunkHeight = afterRestRocksTotalHeight - beforeChunkTotalHeight
  chunkHeight = snd state.highestPoint - beforeChunkTotalHeight

countChunksThatFit ::
  Int -> -- total number of rocks to fall
  Int -> -- chunk size
  Int -> -- rocks before first chunk
  Int -- resulting number of chunks that fit into the total number of rocks to fall
countChunksThatFit total chunkSize start = (total - start) `div` chunkSize

data StreamMove
  = Left
  | Right
  deriving (Show)

type Position = (Int, Int)

newtype Rock = MkRock [Position] deriving (Show) -- Positions from the bottom left corner of the rock profile

rockPositions :: Rock -> [Position]
rockPositions (MkRock pos) = pos

data RockInTower = MkRockInTower
  { x :: Int
  , towerHeightWithoutThis :: Int
  , towerheightWithThis :: Int
  }
  deriving (Show)

data State = MkState
  { blockedTiles :: Set.HashSet Position
  , highestPoint :: Position
  , rockType :: Rock
  , rockPosition :: Position
  , recentBlockedPos :: Seq.Seq Position
  , rocksInTower :: [RockInTower]
  }
  deriving (Show)

data MoveType
  = Fall
  | StremPush
  deriving (Show)

data EnvProps = MkEnvProps
  { fallSize :: Int
  , streamPushSize :: Int
  , roomWidth :: Int
  , maxKeptBlockedPos :: Int -- performance optimization not to grow the set too big (after certain amount of position the old positions become irrelevant)
  }
  deriving (Show)

spawnRock :: (Int, Int) -> CyclicList Rock -> State -> (CyclicList Rock, State)
spawnRock (fromLeft, fromHighestRock) rocks state =
  ( newRocks
  , state
      { rockType = newRock
      , rockPosition = (fromLeft, snd state.highestPoint + fromHighestRock + 1)
      }
  )
 where
  (newRock, newRocks) = CyclicList.readItem rocks

makeRockFallUntilRest :: EnvProps -> CyclicList StreamMove -> State -> (CyclicList StreamMove, State)
makeRockFallUntilRest envProps = go
 where
  go :: CyclicList StreamMove -> State -> (CyclicList StreamMove, State)
  go moves state =
    if didMoveDown state.rockPosition nextState.rockPosition
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
    , highestPoint = newHeighestPoint
    , recentBlockedPos = newRecentBlocked
    , rocksInTower = MkRockInTower{x = fst state.rockPosition, towerHeightWithoutThis = snd state.highestPoint, towerheightWithThis = snd newHeighestPoint} : state.rocksInTower
    }
 where
  (newRecentBlocked, newBlockedSet) = Foldable.foldl' (addBlocked envProps.maxKeptBlockedPos) (state.recentBlockedPos, state.blockedTiles) newBlocked
  newHeighestPoint = Foldable.maximumBy (compare `Fnc.on` snd) (state.highestPoint : newBlocked)

  newBlocked = map (addPos state.rockPosition) (rockPositions state.rockType)

  addBlocked :: Int -> (Seq.Seq Position, Set.HashSet Position) -> Position -> (Seq.Seq Position, Set.HashSet Position)
  addBlocked maxSetSize inputs newBlockedPos = (newBlockedPos :<| recentBlocked, Set.insert newBlockedPos blocked)
   where
    (recentBlocked, blocked) =
      if Seq.length (fst inputs) >= maxSetSize
        then removeLast inputs
        else inputs

  removeLast :: (Seq.Seq Position, Set.HashSet Position) -> (Seq.Seq Position, Set.HashSet Position)
  removeLast (Seq.Empty, s) = (Seq.Empty, s)
  removeLast (prev :|> lastPos, s) = (prev, Set.delete lastPos s)

makeRockFall :: EnvProps -> State -> State
makeRockFall envProps state =
  if isValidPosition envProps state.blockedTiles state.rockType newRockPos
    then state{rockPosition = newRockPos}
    else state
 where
  newRockPos = moveDownBy envProps.fallSize state.rockPosition

pushWithStream :: EnvProps -> StreamMove -> State -> State
pushWithStream envProps streamMove state =
  if isValidPosition envProps state.blockedTiles state.rockType newRockPos
    then state{rockPosition = newRockPos}
    else state
 where
  moveFnc = case streamMove of
    Left -> moveLeftBy
    Right -> moveRightBy

  newRockPos = moveFnc envProps.streamPushSize state.rockPosition

isValidPosition :: EnvProps -> Set.HashSet Position -> Rock -> Position -> Bool
isValidPosition envProps blocked rock bottomLeftPos = all valid (rockPositions rock)
 where
  -- rockPositions rock
  -- & map (addPos bottomLeftPos)
  -- & all (\pos -> doesntBreakBottom pos && doesntBreakLeftWall pos && doesntBreakRightWall pos && doesntCollideWithBlocked pos)

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
  rockCharParser =
    MP.choice
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
  lineToPositions lineNum line = Maybe.mapMaybe (\(k, v) -> if v == rockSymbol then Just (k, lineNum) else Nothing) (zip [0 ..] line)

movesParser :: Parser (CyclicList StreamMove)
movesParser = do
  moves <- MP.some moveParser
  case CyclicList.create moves of
    Prelude.Left err -> fail err
    Prelude.Right res -> pure res

moveParser :: Parser StreamMove
moveParser =
  MP.choice
    [ MP.Char.char '>' >> pure Right
    , MP.Char.char '<' >> pure Left
    ]
