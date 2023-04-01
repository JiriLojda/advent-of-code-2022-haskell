module Day18.SolutionSpec (spec) where

import Data.Function ((&))
import Data.List (intercalate)
import Test.Hspec (Spec, describe, it, shouldBe)

import Day18.Solution (task2)

spec :: Spec
spec = do
  describe "task2" $ do
    it "Ignores single-cube pocket" $ do
      -- pocket = (3, 3, 3)
      let cubes =
            [ (4, 3, 3) -- X
            , (2, 3, 3)
            , (3, 4, 3) -- Y
            , (3, 2, 3)
            , (3, 3, 4) -- Z
            , (3, 3, 2)
            ]
      result <- task2 (Just $ serializeCubes cubes)
      result `shouldBe` (6 * 5) -- 6 cubes bordering the pocket with each 5 exposed sides (one side borders the pocket)
    it "Ignores line-of-cubes pocket" $ do
      -- pocket = (3, 3, 3), (3, 4, 3) (3, 5, 3)
      let cubes =
            [ (4, 3, 3) -- X
            , (2, 3, 3)
            , (4, 4, 3)
            , (2, 4, 3)
            , (4, 5, 3)
            , (2, 5, 3)
            , (3, 6, 3) -- Y
            , (3, 2, 3)
            , (3, 3, 4) -- Z
            , (3, 3, 2)
            , (3, 4, 4)
            , (3, 4, 2)
            , (3, 5, 4)
            , (3, 5, 2)
            ]
      result <- task2 (Just $ serializeCubes cubes)
      result `shouldBe` ((2 * 5) + (2 * 4 * 4) + (4 * 3)) -- 2 exposed cubes (Y coord) + 3 cubes on each other side (2 coords * 2 => 4 sides) 2 cubes bordering cubes from the 3 have one more side exposed outside
    it "Ignores curved-line-of-cubes pocket" $ do
      -- pocket = (3, 3, 3), (3, 4, 3) (4, 4, 3)
      let cubes =
            [ (4, 3, 3) -- X
            , (2, 3, 3)
            , (5, 4, 3)
            , (2, 4, 3)
            , (3, 5, 3) -- Y
            , (4, 5, 3)
            , (3, 2, 3)
            , (3, 3, 4) -- Z
            , (3, 3, 2)
            , (3, 4, 4)
            , (3, 4, 2)
            , (4, 4, 4)
            , (4, 4, 2)
            ]
      result <- task2 (Just $ serializeCubes cubes)
      result `shouldBe` ((2 * 5) + (2 * 2 * 4) + (2 * ((2 * 4) + 3)) + 4) -- (2 ends of the line) + (2 parts of the outer side of the curve) + (2 L-shaped sides each with two borders (4 sides) and one corner (3 sides)) + (inner side of the curve (one cube with 4 sides exposed))
    it "Ignores double-curved-line-of-cubes pocket" $ do
      -- pocket = (3, 3, 3) (3, 4, 3) (4, 4, 3) (4, 4, 4)
      let cubes =
            [ (4, 3, 3) -- X
            , (2, 3, 3)
            , (5, 4, 3)
            , (5, 4, 4)
            , (3, 4, 4)
            , (2, 4, 3)
            , (3, 5, 3) -- Y
            , (4, 5, 3)
            , (3, 5, 4)
            , (4, 5, 4)
            , (3, 2, 3)
            , (4, 3, 4)
            , (3, 3, 4) -- Z
            , (3, 3, 2)
            , (3, 4, 2)
            , (4, 4, 5)
            , (4, 4, 2)
            ]
      result <- task2 (Just $ serializeCubes cubes)
      result `shouldBe` ((2 * 5) + (2 * 8) + (2 * (8 + 3)) + 12) -- (2 ends of the line) + (2 * 2-cube line with one side touching the pocket) + (2 L-shapes with one flat side touching) + (3d snake around the pocket)
    it "Ignores snake-like pocket" $ do
      -- pocket = (3, 3, 3)...(3, 5, 3) (4, 5, 3) (5, 5, 3)...(5, 3, 3) (6, 3, 3) (7, 3, 3)...(7, 5, 3)
      let cubes =
            [(x, y, 3) | x <- [2, 8], y <- [2 .. 6]] -- border around
              ++ [(x, y, 3) | x <- [3 .. 7], y <- [2, 6]]
              ++ [(4, y, 3) | y <- [3 .. 4]] -- walls inside
              ++ [(6, y, 3) | y <- [4 .. 5]]
              ++ [(x, y, z) | x <- [2 .. 8], y <- [2 .. 6], z <- [2, 4]] -- roof from the top and the bottom
      result <- task2 (Just $ serializeCubes cubes)
      result `shouldBe` ((2 * 35) + (6 * 7) + (6 * 5)) -- (top and bottom roof) + (x sides) + (y sides)
    it "Ignores cube pocket" $ do
      -- pocket = (3, 3, 3)...(5, 5, 5)
      let cubes =
            [(x, y, z) | x <- [2 .. 6], y <- [2 .. 6], z <- [2, 6]] -- z sides
              ++ [(x, y, z) | x <- [2 .. 6], y <- [2, 6], z <- [3 .. 5]] -- y sides
              ++ [(x, y, z) | x <- [2, 6], y <- [3 .. 5], z <- [3 .. 5]] -- x sides
      result <- task2 (Just $ serializeCubes cubes)
      result `shouldBe` ((5 * 5) * 6) -- (one side of 5 * 5 sides) * 6 sides of the cube

type Cube = (Int, Int, Int)

serializeCube :: Cube -> String
serializeCube (x, y, z) = show x ++ "," ++ show y ++ "," ++ show z

serializeCubes :: [Cube] -> String
serializeCubes cubes =
  cubes
    & map serializeCube
    & intercalate "\n"
