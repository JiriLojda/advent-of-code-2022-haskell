module Day15.Solution (task1, task2) where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP.Char
import qualified Text.Megaparsec.Char.Lexer as MP.Char.L
import Data.Function ((&))
import qualified Data.Set as Set
import qualified Data.List as List

import qualified Utils
import Utils (Parser)

type Point = (Int, Int)
data SensorData = SensorData
  { position :: Point
  , closestBeacon :: Point
  } deriving Show

task2 :: IO (Maybe Int)
task2 = do
  sensorData <- Utils.loadAndParseFile "src/Day15/data.txt" sensorsParser
  findUndetectedIn (minXAndY, maxXAndY) (minXAndY, maxXAndY) sensorData
    & fmap calculateTuningFrequency
    & pure
  where
    minXAndY = 0
    maxXAndY = 4000000 -- data
    -- maxXAndY = 20 -- sampleData

    calculateTuningFrequency :: Point -> Int
    calculateTuningFrequency (x, y) = (x * 4000000) + y

findUndetectedIn :: (Int, Int) -> (Int, Int) -> [SensorData] -> Maybe Point
findUndetectedIn (minX, maxX) (minY, maxY) sensors = go (minX, minY)
  where
    go :: Point -> Maybe Point
    go (!x, !y) | y > maxY = go (x + 1, minY)
                | x > maxX = Nothing
                | Just sensor <- List.find (flip isExcludedBySensor (x, y)) sensors = go (skipAfterSensorRange (x, y) sensor)
                | otherwise = Just (x, y)
    
    skipAfterSensorRange :: Point -> SensorData -> Point
    skipAfterSensorRange (x, _) sensor = (x, sensorRangeEndY x sensor + 1)

sensorRangeEndY :: Int -> SensorData -> Int
sensorRangeEndY x sensor = snd sensor.position + (sensorRange sensor - horizontalDistance)
  where
    horizontalDistance = pointDistance (x, snd sensor.position) sensor.position

sensorRange :: SensorData -> Int
sensorRange sensor = pointDistance sensor.position sensor.closestBeacon

task1 :: IO Int
task1 = do
  sensorData <- Utils.loadAndParseFile "src/Day15/data.txt" sensorsParser
  pure (length (possibleXs sensorData))
  where
    y = 2000000 -- data
    -- y = 10 -- sampleData

    possibleXs sensorData = Set.toList $ Set.fromList (sensorData >>= getPossibleXsFromSensor)

    getPossibleXsFromSensor :: SensorData -> [Int]
    getPossibleXsFromSensor sensor@SensorData{position = (sensorCenterX, _)} =
      [(sensorCenterX - sensorRange sensor)..(sensorCenterX + sensorRange sensor)]
        & map (, y)
        & filter (isExcludedBySensor sensor)
        & map fst

isExcludedBySensor :: SensorData -> Point -> Bool
isExcludedBySensor sensor point =
  (pointDistance point sensor.position <= pointDistance sensor.position sensor.closestBeacon) &&
  point /= sensor.closestBeacon

pointDistance :: Point -> Point -> Int
pointDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Parsers

sensorsParser :: Parser [SensorData]
sensorsParser = sensorParser `MP.sepEndBy1` MP.Char.newline

sensorParser :: Parser SensorData
sensorParser = do
  _ <- MP.Char.string "Sensor at "
  position <- pointParser
  _ <- MP.Char.string ": closest beacon is at "
  closestBeacon <- pointParser
  pure (SensorData {position, closestBeacon})

pointParser :: Parser Point
pointParser = do
  _ <- MP.Char.string "x="
  x <- MP.Char.L.signed mempty MP.Char.L.decimal
  _ <- MP.Char.char ','
  _ <- MP.Char.space
  _ <- MP.Char.string "y="
  y <- MP.Char.L.signed mempty MP.Char.L.decimal
  pure (x, y)
