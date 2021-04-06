module Terrain where

import Data.Char
import Data.Functor (($>))
import Data.Maybe
import Data.Vector (Vector, fromList, (!), length, elemIndex, findIndex, elem)
import Data.Void
import Prelude hiding (length) -- we need Vectors length, not Lists
import Text.Megaparsec (runParser, Parsec, (<|>), many)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Error (errorBundlePretty)

newtype X = X Int deriving (Show, Eq, Ord)
newtype Y = Y Int deriving (Show, Eq, Ord)

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
data Pos = Pos { x :: X, y :: Y } deriving (Show, Eq, Ord)

-- Different tile types on the terrain
data Tile = Start | Goal | On | Off deriving (Show, Eq)

-- LevelVector that defines where tiles are is represented as 2d Vector
--
type LevelVector = Vector (Vector Tile)

-- The terrain is represented as a function from positions to
-- booleans. The function returns `True` for every position that
-- is inside the terrain.
--
-- As explained in the documentation of type `Pos`, the `x` axis
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
buildLevel rows = toLevel $ toTerrain rows

toTerrain :: [String] -> LevelVector
toTerrain rows = fromList $ map parseRow rows

type Parser = Parsec Void String

-- Parses a row of terrain into tiles
parseRow :: String -> Vector Tile
parseRow row = 
    case runParser parser "" row of
        (Left e)        -> error $ errorBundlePretty e
        (Right tiles)   -> tiles
    where
        parser :: Parser (Vector Tile)
        parser = fromList <$> many tile
        
        tile :: Parser Tile
        tile = start <|> goal <|> on <|> off
        
        start   = char 'S' $> Start
        goal    = char 'T' $> Goal
        on      = char 'o' $> On
        off     = char '-' $> Off

toLevel :: LevelVector -> Level
toLevel t = Level { start = findTile t Start
                  , goal = findTile t Goal
                  , terr = terrain t
                  }

-- TODO 1:
-- This function should return the position of tile `t` in the
-- terrain described by `LevelVector`. You can assume that the `t`
-- appears exactly once in the terrain.
--
-- Hint: you can use the functions `findIndex` and / or `elemIndex` of the
-- `Vector` type or use the following definitions that get rid of the
-- Maybe wrapper (not really relevant in this task).
findTile :: LevelVector -> Tile -> Pos
findTile levelVector t = undefined

findIndex' :: (Vector Tile -> Bool) -> LevelVector -> Int
findIndex' f vector = fromJust $ findIndex f vector

elemIndex' :: Tile -> Vector Tile -> Int
elemIndex' e vector = fromJust $ elemIndex e vector

-- TODO 2:
-- This method returns terrain function that represents the terrain
-- in `LevelVector`. The vector contains parsed version of the `level`
-- string. For example, the following level
--
-- level0 = ["ST",
--           "oo",
--           "oo"]
--
-- is represented as
--
--   Vector(Vector(Start, Goal), Vector(On, On), Vector(On, On))
--
-- The resulting function should return `True` if the position `pos` is
-- a valid position (not Off) inside the terrain described
-- by `LevelVector`.
terrain :: LevelVector -> Terrain
terrain levelVector pos = undefined
