{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Day19.Solution (task1, task2) where

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP.Char
import Text.Megaparsec.Char.Lexer qualified as MP.Char.L

import Control.Monad qualified as Monad
import Data.Function ((&))
import Utils (Parser)
import Utils qualified

task2 :: IO String
task2 = pure "Not implemented"

task1 :: IO String
task1 = do
  blueprints <- Utils.loadAndParseFile "src/Day19/sampleData.txt" blueprintsParser
  print (map maxGeodeProductionNaive blueprints)
  pure "Not implemented"

data PerResourceCount = MkPerResourceCount
  { ore :: Int
  , clay :: Int
  , obsidian :: Int
  , geode :: Int
  }

emptyResourceCounts :: PerResourceCount
emptyResourceCounts =
  MkPerResourceCount
    { ore = 0
    , clay = 0
    , obsidian = 0
    , geode = 0
    }

data State = MkState
  { production :: PerResourceCount
  , stockpile :: PerResourceCount
  , minutesLeft :: Int
  }

maxGeodeProductionNaive :: Blueprint -> Int
maxGeodeProductionNaive blueprint = go initialState
 where
  initialState = MkState{production = emptyResourceCounts{ore = 1}, stockpile = emptyResourceCounts, minutesLeft = 24}

  go :: State -> Int
  go state
    | state.minutesLeft <= 0 = state.stockpile.geode
    | otherwise =
        [ [id]
        , [buildOreRobot | canBuildOreRobot state.stockpile]
        , [buildClayRobot | canBuildClayRobot state.stockpile]
        , [buildObsidianRobot | canBuildObsidianRobot state.stockpile]
        , [buildGeodeRobot | canBuildGeodeRobot state.stockpile]
        ]
          & concat
          & map (go . (`moveByMinute` state))
          & maximum

  moveByMinute :: (PerResourceCount -> PerResourceCount) -> State -> State
  moveByMinute changeProduction state =
    MkState
      { production = changeProduction state.production
      , stockpile = sumResources state.production state.stockpile
      , minutesLeft = state.minutesLeft - 1
      }
   where
    sumResources :: PerResourceCount -> PerResourceCount -> PerResourceCount
    sumResources r1 r2 = MkPerResourceCount{ore = r1.ore + r2.ore, clay = r1.clay + r2.clay, obsidian = r1.obsidian + r2.obsidian, geode = r1.geode + r2.geode}

  buildOreRobot :: PerResourceCount -> PerResourceCount
  buildOreRobot production = production{ore = production.ore + 1}

  buildClayRobot :: PerResourceCount -> PerResourceCount
  buildClayRobot production = production{clay = production.clay + 1}

  buildObsidianRobot :: PerResourceCount -> PerResourceCount
  buildObsidianRobot production = production{obsidian = production.obsidian + 1}

  buildGeodeRobot :: PerResourceCount -> PerResourceCount
  buildGeodeRobot production = production{geode = production.geode + 1}

  canBuildOreRobot :: PerResourceCount -> Bool
  canBuildOreRobot stock = stock.ore >= blueprint.oreRobotCost

  canBuildClayRobot :: PerResourceCount -> Bool
  canBuildClayRobot stock = stock.ore >= blueprint.clayRobotCost

  canBuildObsidianRobot :: PerResourceCount -> Bool
  canBuildObsidianRobot stock = stock.ore >= blueprint.obsidianRobotCost.ore && stock.clay >= blueprint.obsidianRobotCost.clay

  canBuildGeodeRobot :: PerResourceCount -> Bool
  canBuildGeodeRobot stock = stock.ore >= blueprint.geodeRobotCost.ore && stock.obsidian >= blueprint.geodeRobotCost.obsidian

data ObsidianRobotCost = MkObsidianRobotCost
  { ore :: Int
  , clay :: Int
  }
  deriving (Show)

data GeodeRobotCost = MkGeodeRobotCost
  { ore :: Int
  , obsidian :: Int
  }
  deriving (Show)

data Blueprint = MkBlueprint
  { id :: Int
  , oreRobotCost :: Int
  , clayRobotCost :: Int
  , obsidianRobotCost :: ObsidianRobotCost
  , geodeRobotCost :: GeodeRobotCost
  }
  deriving (Show)

robotCostParser :: String -> Parser cost -> Parser cost
robotCostParser robotName costParser = do
  _ <- MP.Char.string "Each "
  _ <- MP.Char.string robotName
  _ <- MP.Char.string " robot costs "
  cost <- costParser
  _ <- MP.Char.char '.'
  pure cost

resourceCostParser :: String -> Parser Int
resourceCostParser resourceName = do
  cost <- MP.Char.L.decimal
  _ <- MP.Char.char ' '
  _ <- MP.Char.string resourceName
  pure cost

blueprintParser :: Parser Blueprint
blueprintParser = do
  _ <- MP.Char.string "Blueprint "
  blueprintId <- MP.Char.L.decimal
  _ <- MP.Char.string ": "
  oreRobotCost <- robotCostParser "ore" $ resourceCostParser "ore"
  _ <- MP.Char.char ' '
  clayRobotCost <- robotCostParser "clay" $ resourceCostParser "ore"
  _ <- MP.Char.char ' '
  obsidianRobotCost <- robotCostParser "obsidian" (MkObsidianRobotCost <$> resourceCostParser "ore" <*> (andParser >> resourceCostParser "clay"))
  _ <- MP.Char.char ' '
  geodeRobotCost <- robotCostParser "geode" (MkGeodeRobotCost <$> resourceCostParser "ore" <*> (andParser >> resourceCostParser "obsidian"))
  pure $ MkBlueprint{id = blueprintId, oreRobotCost, clayRobotCost, obsidianRobotCost, geodeRobotCost}
 where
  andParser :: Parser ()
  andParser = Monad.void $ MP.Char.string " and "

blueprintsParser :: Parser [Blueprint]
blueprintsParser = blueprintParser `MP.sepEndBy` MP.Char.newline
