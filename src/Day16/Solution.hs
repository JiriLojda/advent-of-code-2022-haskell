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

task2 :: IO String
task2 = pure "Not implemented yet"

task1 :: IO Int
task1 = do
  valvesList <- Utils.loadAndParseFile "src/Day16/data.txt" valvesParser
  let valveEntries = map (\v -> (v.name, v)) valvesList
      valves = Map.fromList valveEntries
      bestActions = findBestActionsInMinutes 30 "AA" valves
      releasedSum = actionsValue valves 30 bestActions
  pure releasedSum

minutesForTransition :: Int
minutesForTransition = 1

minutesToOpen :: Int
minutesToOpen = 1

findBestActionsInMinutes :: Int -> ValveName -> Valves -> [Action]
findBestActionsInMinutes maxMinutes valveName valves = go [(valveName, [], maxMinutes, 0, mempty)] ([], 0)
  where
    valveFromName :: ValveName -> Valve
    valveFromName vName = Maybe.fromMaybe (error ("Cannot find actions for non-existent valve. Valve name: " ++ vName)) (Map.lookup vName valves)

    isNonEmptyValve :: ValveName -> Bool
    isNonEmptyValve vName = maybe False ((> 0) . (.flowRate)) (Map.lookup vName valves)

    !pathsBetweenValves =
      valves
        & Map.keys
        & filter (\v -> isNonEmptyValve v || v == valveName)
        & map (\v -> (v, findPathsFromValve v valves))
        & map (BiF.second (Map.filterWithKey $ \k _ -> isNonEmptyValve k))
        & Map.fromList
    
    maxVisited =
      valves
        & Map.keys
        & filter isNonEmptyValve
        & length

    go :: [(ValveName, [Action], Int, Int, Set.Set ValveName)] -> ([Action], Int) -> [Action]
    go [] (best, _) = reverse best
    go ((vName, actions, !minutesLeft, !pressureSoFar, !visited) : restFrames) currentBest@(_, currentBestPressure) =
      if minutesLeft <= 1 || Set.size visited == maxVisited || null (createNextFrames currentValve)
        then go restFrames (if pressureSoFar > currentBestPressure then (actions, pressureSoFar) else currentBest)
        else go (createNextFrames currentValve ++ restFrames) currentBest
      where
        currentValve = valveFromName vName

        createNextFrames :: Valve -> [(ValveName, [Action], Int, Int, Set.Set ValveName)]
        createNextFrames v = do
          (neighbour, (dist, path)) <- maybe [] (map (BiF.first valveFromName) . Map.toList) (Map.lookup v.name pathsBetweenValves)
          let newMinutesLeft = minutesLeft - (dist * minutesForTransition) - minutesToOpen
              newPressure = pressureSoFar + (newMinutesLeft * neighbour.flowRate)
              newActions = Open neighbour.name : map MoveTo path ++ actions
          Monad.guard (newMinutesLeft >= 0 && Set.notMember neighbour.name visited)
          pure (neighbour.name, newActions, newMinutesLeft, newPressure, Set.insert v.name visited)
        
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
