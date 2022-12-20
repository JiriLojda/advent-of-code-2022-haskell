module Day14.CommonTypes
  ( Position
  , Column
  ) where

import Data.Set (Set)

type Column = Set Int
type Position = (Int, Int)
