module Terrain where

import Data.Char
import Data.Maybe
import Data.Vector (Vector, fromList, (!), length, elemIndex, findIndex, elem)
import Prelude hiding (length) -- we need Vectors length, not lists


-- The data type `Pos` encodes positions in the terrain.
--
-- IMPORTANT NOTE
--  - The `x` coordinate denotes the position on the vertical axis
--  - The `y` coordinate is used for the horizontal axis
--  - The coordinates increase when moving down and right
--
-- Illustration:
--
--     0 1 2 3   <- y axis
--   0 o o o o
--   1 o o o o
--   2 o # o o    # is at position Pos(2, 1)
--   3 o o o o
--
--   ^
--   |
--
--   x axis
data Pos = Pos { x :: Int, y :: Int } deriving (Show, Eq, Ord)

type RawLevel = Vector (Vector Char)
-- The terrain is represented as a function from positions to
-- booleans. The function returns `true` for every position that
-- is inside the terrain.
--
-- As explained in the documentation of class `Pos`, the `x` axis
-- is the vertical one and increases from top to bottom.
type Terrain = Pos -> Bool

data Level = Level { start :: Pos
                   , goal :: Pos
                   , terr :: Terrain
                   }

-- This component implements a parser to define terrains from a
-- graphical ASCII representation.
--
-- - The `-` character denotes parts which are outside the terrain
-- - `o` denotes fields which are part of the terrain
-- - `S` denotes the start position of the block (which is also considered
--    inside the terrain)
-- - `T` denotes the final position of the block (which is also considered
--    inside the terrain)
buildLevel :: [String] -> Level
buildLevel = toLevel . toTerrain

toTerrain :: [String] -> RawLevel
toTerrain = fromList . map fromList

toLevel :: RawLevel -> Level
toLevel t = Level { start = findChar 'S' t
                  , goal = findChar 'T' t
                  , terr = terrain t
                  }

-- TODO 1:
-- This function should return the position of character `c` in the
-- terrain described by `levelVector`. You can assume that the `c`
-- appears exactly once in the terrain.
--
-- Hint: you can use the functions `findIndex` and / or `elemIndex` of the
-- `Vector` class
findChar :: Char -> RawLevel -> Pos
findChar c levelVector = undefined

-- TODO 2:
-- This method returns terrain function that represents the terrain
-- in `levelVector`. The vector contains parsed version of the `level`
-- string. For example, the following level
--
-- level0 = ["ST",
--           "oo",
--           "oo"]
--
-- is represented as
--
--   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
--
-- The resulting function should return `true` if the position `pos` is
-- a valid position (not a '-' character) inside the terrain described
-- by `levelVector`.
terrain :: RawLevel -> Terrain
terrain levelVector pos = undefined
