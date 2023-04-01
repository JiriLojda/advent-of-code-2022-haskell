module Day18.Solution (
  task1,
  task2,
) where

import Data.Function ((&))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP.Char
import Text.Megaparsec.Char.Lexer qualified as MP.Char.L

import Data.Maybe qualified as Maybe
import Utils (Parser)
import Utils qualified

task2 :: Maybe String -> IO Int
task2 mayData = do
  cubes <- maybe (Utils.loadAndParseFile "src/Day18/data.txt" cubesParser) (Utils.parseString cubesParser) mayData

  pure (calculateTotalSurfaceWithoutPockets cubes)

calculateTotalSurfaceWithoutPockets :: [CubePosition] -> Int
calculateTotalSurfaceWithoutPockets initialCubes = fst (List.foldl' addCube (0, mempty) initialCubes)
 where
  cubes = createCubesByCoordinate initialCubes

  addCube :: (Int, Set.Set CubePosition) -> CubePosition -> (Int, Set.Set CubePosition)
  addCube initialState cube =
    [(dir, coord, move dir coord cube) | dir <- [Up, Down], coord <- [X, Y, Z]] -- all cube sides
      & List.foldl' addSide initialState

  addSide :: (Int, Set.Set CubePosition) -> (Direction, Coord, CubePosition) -> (Int, Set.Set CubePosition)
  addSide (total, knownPockets) (dir, coord, pos) = (if blocked then total else total + 1, newKnownPockets)
   where
    (blocked, newKnownPockets) = isBlocked knownPockets dir coord pos

  move :: Direction -> Coord -> CubePosition -> CubePosition
  move direction = updateCoord (+ change)
   where
    change = case direction of
      Up -> 1
      Down -> -1

  isBlocked :: Set.Set CubePosition -> Direction -> Coord -> CubePosition -> (Bool, Set.Set CubePosition)
  isBlocked knownPockets direction coord pos
    | isCube || Set.member pos knownPockets = (True, knownPockets)
    | otherwise = case findPocket cubes direction coord pos of
        Just newPocket -> (True, Set.union newPocket knownPockets)
        Nothing -> (False, knownPockets)
   where
    isCube =
      Map.lookup (coordGetter coord pos) (coordCubes coord cubes)
        & Maybe.fromMaybe []
        & elem pos

newtype CubesByCoordinate = MkCubesByCoordinate (Map.Map Int [CubePosition], Map.Map Int [CubePosition], Map.Map Int [CubePosition]) deriving (Show)

data Coord
  = X
  | Y
  | Z
  deriving (Show)

otherDirections :: Coord -> [Coord]
otherDirections X = [Y, Z]
otherDirections Y = [X, Z]
otherDirections Z = [X, Y]

coordGetter :: Coord -> (CubePosition -> Int)
coordGetter X = cubeX
coordGetter Y = cubeY
coordGetter Z = cubeZ

updateCoord :: (Int -> Int) -> Coord -> CubePosition -> CubePosition
updateCoord updater X pos = MkCubePosition (updater (cubeX pos), cubeY pos, cubeZ pos)
updateCoord updater Y pos = MkCubePosition (cubeX pos, updater (cubeY pos), cubeZ pos)
updateCoord updater Z pos = MkCubePosition (cubeX pos, cubeY pos, updater (cubeZ pos))

coordCubes :: Coord -> CubesByCoordinate -> Map.Map Int [CubePosition]
coordCubes X (MkCubesByCoordinate (res, _, _)) = res
coordCubes Y (MkCubesByCoordinate (_, res, _)) = res
coordCubes Z (MkCubesByCoordinate (_, _, res)) = res

data Direction
  = Up
  | Down
  deriving (Show)

isInDirection :: Direction -> Int -> Int -> Bool
isInDirection Up = (>)
isInDirection Down = (<)

findPocket :: CubesByCoordinate -> Direction -> Coord -> CubePosition -> Maybe (Set.Set CubePosition)
findPocket cubesByCoordinate dirIntoPocket touchingCoord startingCube = do
  let otherDirs = otherDirections touchingCoord
  firstOtherDir <- Utils.head otherDirs
  sameCoordCubes <- Map.lookup (coordGetter firstOtherDir startingCube) (coordCubes firstOtherDir cubesByCoordinate)
  let filtered = filter (\c -> all (\d -> coordGetter d c == coordGetter d startingCube) otherDirs) sameCoordCubes
  upperBlocker <- List.find (\c -> isInDirection dirIntoPocket (coordGetter touchingCoord c) (coordGetter touchingCoord startingCube)) filtered
  go mempty ((touchingCoord, startingCube) : map (touchingCoord,) (cubesBetween touchingCoord startingCube upperBlocker))
 where
  go :: Set.Set CubePosition -> [(Coord, CubePosition)] -> Maybe (Set.Set CubePosition)
  go visited [] = Just visited
  go visited ((coord, currentPos) : restPos)
    | Set.member currentPos visited = go visited restPos
    | otherwise = case map (\d -> (d, findBlockers (coordGetter d) (coordGetter d currentPos) (cubesOnSameCoordinate d currentPos))) (otherDirections coord) of
        [(c1, Just blockers1), (c2, Just blockers2)] -> go (Set.insert currentPos visited) (toVisit c1 blockers1 ++ toVisit c2 blockers2 ++ restPos)
        _ -> Nothing
   where
    toVisit :: Coord -> (CubePosition, CubePosition) -> [(Coord, CubePosition)]
    toVisit c (lowerCube, upperCube) = map (c,) (cubesBetween c lowerCube currentPos ++ cubesBetween c currentPos upperCube)

  cubesOnSameCoordinate :: Coord -> CubePosition -> [CubePosition]
  cubesOnSameCoordinate coord cube = Maybe.fromMaybe [] $ do
    let otherDirs = otherDirections coord
    otherDir <- Utils.head otherDirs
    potentialCubes <- Map.lookup (coordGetter otherDir cube) (coordCubes otherDir cubesByCoordinate)
    pure $ filter (\c -> all (\d -> coordGetter d c == coordGetter d cube) otherDirs) potentialCubes

  cubesBetween :: Coord -> CubePosition -> CubePosition -> [CubePosition] -- other coords (other than the Coord param) has to be the same
  cubesBetween coord c1 c2 = case coord of
    X -> map (\x -> MkCubePosition (x, cubeY c1, cubeZ c1)) [lowerBound cubeX c1 c2 .. upperBound cubeX c1 c2]
    Y -> map (\y -> MkCubePosition (cubeX c1, y, cubeZ c1)) [lowerBound cubeY c1 c2 .. upperBound cubeY c1 c2]
    Z -> map (\z -> MkCubePosition (cubeX c1, cubeY c1, z)) [lowerBound cubeZ c1 c2 .. upperBound cubeZ c1 c2]
   where
    lowerBound :: (CubePosition -> Int) -> CubePosition -> CubePosition -> Int
    lowerBound getter cube1 cube2 = min (getter cube1) (getter cube2) + 1

    upperBound :: (CubePosition -> Int) -> CubePosition -> CubePosition -> Int
    upperBound getter cube1 cube2 = max (getter cube1) (getter cube2) - 1

  findBlockers :: (CubePosition -> Int) -> Int -> [CubePosition] -> Maybe (CubePosition, CubePosition)
  findBlockers cubeCoord coord cubesOnCoordinate = do
    let lowerCubes = takeWhile ((< coord) . cubeCoord) $ List.sortOn cubeCoord cubesOnCoordinate
        higherCubes = dropWhile ((<= coord) . cubeCoord) $ List.sortOn cubeCoord cubesOnCoordinate
    lowerBlocker <- Utils.last lowerCubes
    higherBlocker <- Utils.head higherCubes
    pure (lowerBlocker, higherBlocker)

createCubesByCoordinate :: [CubePosition] -> CubesByCoordinate
createCubesByCoordinate = List.foldl' addCube (MkCubesByCoordinate (mempty, mempty, mempty))
 where
  addCube :: CubesByCoordinate -> CubePosition -> CubesByCoordinate
  addCube (MkCubesByCoordinate (byX, byY, byZ)) pos@(MkCubePosition (x, y, z)) = MkCubesByCoordinate (newByX, newByY, newByZ)
   where
    newByX = Map.insertWith (++) x [pos] byX
    newByY = Map.insertWith (++) y [pos] byY
    newByZ = Map.insertWith (++) z [pos] byZ

task1 :: IO Int
task1 = do
  cubes <- Utils.loadAndParseFile "src/Day18/data.txt" cubesParser

  pure (calculateTotalSurface cubes)

newtype CubePosition = MkCubePosition (Int, Int, Int) deriving (Eq, Ord, Show)

cubeX :: CubePosition -> Int
cubeX (MkCubePosition (x, _, _)) = x

cubeY :: CubePosition -> Int
cubeY (MkCubePosition (_, y, _)) = y

cubeZ :: CubePosition -> Int
cubeZ (MkCubePosition (_, _, z)) = z

calculateTotalSurface :: [CubePosition] -> Int
calculateTotalSurface cubes = List.foldl' go 0 cubes
 where
  go :: Int -> CubePosition -> Int
  go currentTotal currentPos = currentTotal + (6 - adjacentCount)
   where
    adjacentCount =
      cubes
        & filter (\x -> x /= currentPos && areAdjacent x currentPos)
        & length

areAdjacent :: CubePosition -> CubePosition -> Bool
areAdjacent (MkCubePosition (x1, y1, z1)) (MkCubePosition (x2, y2, z2)) = hasTwoSameCoords && isCoordDistMax1
 where
  coordTuples = [(x1, x2), (y1, y2), (z1, z2)]

  isCoordDistMax1 = all ((<= 1) . abs . uncurry (-)) coordTuples

  hasTwoSameCoords =
    coordTuples
      & filter (uncurry (==))
      & length
      & (== 2)

cubesParser :: Parser [CubePosition]
cubesParser = cubeParser `MP.sepEndBy` MP.Char.newline

cubeParser :: Parser CubePosition
cubeParser = do
  x <- MP.Char.L.decimal
  _ <- MP.Char.char ','
  y <- MP.Char.L.decimal
  _ <- MP.Char.char ','
  z <- MP.Char.L.decimal
  pure (MkCubePosition (x, y, z))
