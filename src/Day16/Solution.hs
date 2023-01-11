module Day16.Solution (task1, task2) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP.Char
import qualified Text.Megaparsec.Char.Lexer as MP.Char.L
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Bifunctor as BiF
import qualified Control.Monad as Monad

import qualified Utils
import Utils (Parser)

type ValveName = String

data Valve = MkValve
  { flowRate :: Int
  , tunnelsTo :: [ValveName]
  , name :: ValveName
  } deriving Show

type Valves = Map.Map ValveName Valve

data Action
  = MoveTo ValveName
  | Open ValveName
  deriving Show

task2 :: IO Int
task2 = do
  valvesList <- Utils.loadAndParseFile "src/Day16/data.txt" valvesParser
  let valveEntries = map (\v -> (v.name, v)) valvesList
      valves = Map.fromList valveEntries
      maxMinutes = 26
      bestActions = findBestActionsInMinutes maxMinutes ["AA", "AA"] valves
      releasedSum = map (actionsValue valves maxMinutes) bestActions
  pure (sum releasedSum)

task1 :: IO Int
task1 = do
  valvesList <- Utils.loadAndParseFile "src/Day16/data.txt" valvesParser
  let valveEntries = map (\v -> (v.name, v)) valvesList
      valves = Map.fromList valveEntries
      maxMinutes = 30
      bestActions = findBestActionsInMinutes maxMinutes ["AA"] valves
      releasedSum = map (actionsValue valves maxMinutes) bestActions
  pure (sum releasedSum)

minutesForTransition :: Int
minutesForTransition = 1

minutesToOpen :: Int
minutesToOpen = 1

-- type Frame = (ValveName, [Action], Int, Int, Set.Set ValveName)
data Frame = MkFrame
  { valve :: ValveName
  , actions :: [Action]
  , minutesLeft :: Int
  , pressure :: Int
  , isFinished :: Bool
  }

findBestActionsInMinutes :: Int -> [ValveName] -> Valves -> [[Action]]
findBestActionsInMinutes maxMinutes valveNames valves =
  go [(map (\vName -> MkFrame { valve = vName, actions = [], minutesLeft = maxMinutes, pressure = 0, isFinished = False }) valveNames, mempty)] ([], 0)
  where
    valveFromName :: ValveName -> Valve
    valveFromName vName = Maybe.fromMaybe (error ("Cannot find actions for non-existent valve. Valve name: " ++ vName)) (Map.lookup vName valves)

    isNonEmptyValve :: ValveName -> Bool
    isNonEmptyValve vName = maybe False ((> 0) . (.flowRate)) (Map.lookup vName valves)

    !pathsBetweenValves =
      valves
        & Map.keys
        & filter (\v -> isNonEmptyValve v || (v `elem` valveNames))
        & map (\v -> (v, findPathsFromValve v valves))
        & map (BiF.second (Map.filterWithKey $ \k _ -> isNonEmptyValve k))
        & Map.fromList
    
    createNextFrames :: Set.Set ValveName -> Frame -> [(Set.Set ValveName, Frame)]
    createNextFrames visited frame = do
      let valve = valveFromName frame.valve
      (neighbour, (dist, path)) <- maybe [] (map (BiF.first valveFromName) . Map.toList) (Map.lookup valve.name pathsBetweenValves)
      let newMinutesLeft = frame.minutesLeft - (dist * minutesForTransition) - minutesToOpen
          newPressure = frame.pressure + (newMinutesLeft * neighbour.flowRate)
          newActions = Open neighbour.name : map MoveTo path ++ frame.actions
      Monad.guard (newMinutesLeft >= 0 && Set.notMember neighbour.name visited)
      pure (Set.insert neighbour.name visited, MkFrame
        { valve = neighbour.name
        , actions = newActions
        , minutesLeft = newMinutesLeft
        , pressure = newPressure
        , isFinished = False
        })
    
    createNextFrameLists :: Set.Set ValveName -> [Frame] -> [([Frame], Set.Set ValveName)]
    createNextFrameLists _ [] = []
    createNextFrameLists visited (frame : restFrames) =
      if frame.isFinished || null nextFrames
        then map (BiF.first (frame { isFinished = True } :)) (fallbackedNextRestFrames visited)
        else do
          (nextVisited, nextFrame) <- nextFrames
          (nextRestFrames, finalVisited) <- fallbackedNextRestFrames nextVisited
          pure (nextFrame : nextRestFrames, finalVisited)
      where
        nextFrames = createNextFrames visited frame
        allNextRestFrames v = createNextFrameLists v restFrames
        fallbackedNextRestFrames v = if null (allNextRestFrames v) then [([], v)] else allNextRestFrames v

    go :: [([Frame], Set.Set ValveName)] -> ([[Action]], Int) -> [[Action]]
    go [] (best, _) = map reverse best
    go ((frames, visited) : restFrames) currentBest@(_, currentBestPressure) =
      if all (.isFinished) frames
        then go restFrames (if framesPressure > currentBestPressure then (framesActions, framesPressure) else currentBest)
        else go (createNextFrameLists visited frames ++ restFrames) currentBest
      where
        framesPressure = sum . map (.pressure) $ frames
        framesActions = map (.actions) frames
        
type ValvePaths = Map.Map ValveName (Int, [ValveName])

findPathsFromValve :: ValveName -> Valves -> ValvePaths
findPathsFromValve startingValveName valves =
  go ([(startingValveName, startingValveName)], []) mempty 0 mempty
  where
    go :: ([(ValveName, ValveName)], [(ValveName, ValveName)]) -> Set.Set ValveName -> Int -> ValvePaths -> ValvePaths
    go ([], []) _ _ !paths = paths
    go ([], nextRow) !visited !distance !paths = go (nextRow, []) visited (distance + 1) paths
    go ((valveName, predecessorName) : restValves, nextRow) !visited !distance !paths
      | Set.member valveName visited    = go (restValves, nextRow) visited distance paths
      | valveName == startingValveName  = go (restValves, nextValves ++ nextRow) (Set.insert valveName visited) distance paths
      | otherwise                       = go (restValves, nextValves ++ nextRow) (Set.insert valveName visited) distance newPaths
      where
        nextValves = valves
          & Map.lookup valveName
          & fmap (.tunnelsTo)
          & Maybe.fromMaybe []
          & map (, valveName)
        
        newPaths = Map.insert valveName (distance, path) paths
        
        path = paths
          & Map.lookup predecessorName
          & fmap snd
          & Maybe.fromMaybe []
          & (valveName :)
        
actionsValue :: Valves -> Int -> [Action] -> Int
actionsValue valves maxTime actions = go 0 actions 0
  where
    timeFromIndex :: Int -> Int
    timeFromIndex i = max 0 (maxTime - i)

    go :: Int -> [Action] -> Int -> Int
    go _ [] !acc = acc
    go !i (action : restActions) !acc =
      case action of
        MoveTo _ -> go (i + 1) restActions acc
        Open valveName -> case Map.lookup valveName valves of
          Nothing -> error $ "Action leads to a non-existent valve. Valve name: " ++ valveName
          Just valve -> go (i + 1) restActions (acc + (valve.flowRate * timeFromIndex (i + 1)))

-- Parsers

valvesParser :: Parser [Valve]
valvesParser = valveParser `MP.sepEndBy` MP.Char.newline

valveParser :: Parser Valve
valveParser = do
  _ <- MP.Char.string "Valve "
  name <- valveNameParser
  _ <- MP.Char.string " has flow rate="
  flowRate <- MP.Char.L.decimal
  _ <- MP.Char.char ';'
  _ <- MP.Char.space
  tunnelsTo <- MP.choice
    [ MP.Char.string "tunnels lead to valves " >> (valveNameParser `MP.sepBy1` MP.Char.string ", ")
    , MP.Char.string "tunnel leads to valve " >> fmap (: []) valveNameParser
    ]
  pure (MkValve {name, flowRate, tunnelsTo})
  where
    valveNameParser :: Parser ValveName
    valveNameParser = MP.some MP.Char.upperChar
