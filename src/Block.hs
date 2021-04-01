module Block where

import Control.Exception (assert)
import Terrain
import Prelude hiding (Right, Left)

-- In Bloxorz, we can move left, right, Up or down.
-- These moves are encoded as constructors for Move data type.
data Move = Up | Down | Left | Right deriving (Eq, Show)

-- A block is represented by the position of the two cubes that
-- it consists of.
data Block = B { p1 :: Pos, p2 :: Pos } deriving (Ord, Eq, Show)

-- We make sure that `b1` is lexicographically
-- smaller than `b2`.
makeBlock :: Pos -> Pos -> Block
makeBlock p1 p2 = assert (x p1 <= x p2 && y p1 <= y p2) $ B p1 p2

-- The position obtained by changing the `y` coordinate by `d`
dy :: Pos -> Y -> Pos
dy pos@(Pos _ (Y y')) (Y dy) = pos { y = Y $ y' + dy }

-- The position obtained by changing the `x` coordinate by `d`
dx :: Pos -> X -> Pos
dx pos@(Pos (X x') _) (X dx) = pos { x = X $ x' + dx }

-- Returns a block where the `y` coordinates of `p1` and `p2` are
-- changed by `d1` and `d2`, respectively.
blockDy :: Block -> Y -> Y -> Block
blockDy b d1 d2 = makeBlock (dy (p1 b) d1) (dy (p2 b) d2)

-- Returns a block where the `x` coordinates of `p1` and `p2` are
-- changed by `d1` and `d2`, respectively.
blockDx :: Block -> X -> X -> Block
blockDx b d1 d2 = makeBlock (dx (p1 b) d1) (dx (p2 b) d2)

-- TODO 3:
-- Returns `True` if the block is standing.
standing :: Block -> Bool
standing block = undefined

horizontal :: Block -> Bool
horizontal block = (x $ p1 block) == (x $ p2 block)

-- TODO 4:
-- Returns `True` if the block is entirely inside the terrain.
isLegal :: Terrain -> Block ->  Bool
isLegal terrain block = undefined

-- TODO 5:
-- This function returns the block at the start position of
-- the game.
startBlock :: Level -> Block
startBlock level = undefined

-- Functions left, right, up and down encode block moves
-- in Bloxorz game.

left :: Block -> Block
left block
  | standing block   = blockDy block (Y (-2)) (Y (-1))
  | horizontal block = blockDy block (Y (-1)) (Y (-2))
  | otherwise        = blockDy block (Y (-1)) (Y (-1))

right :: Block -> Block
right block
  | standing block   = blockDy block (Y 1) (Y 2)
  | horizontal block = blockDy block (Y 2) (Y 1)
  | otherwise        = blockDy block (Y 1) (Y 1)

up :: Block -> Block
up block
  | standing block   = blockDx block (X (-2)) (X (-1))
  | horizontal block = blockDx block (X (-1)) (X (-1))
  | otherwise        = blockDx block (X (-1)) (X (-2))

down :: Block -> Block
down block
  | standing block   = blockDx block (X 1) (X 2)
  | horizontal block = blockDx block (X 1) (X 1)
  | otherwise        = blockDx block (X 2) (X 1)

-- TODO 6:
-- Returns the list of blocks that can be obtained by moving
-- the current block, together with the corresponding move.
neighbours :: Block -> [(Block, Move)]
neighbours block = undefined

-- TODO 7:
-- Returns the list of positions reachable from the current block
-- which are inside the terrain.
legalNeighbours :: Terrain -> Block ->  [(Block, Move)]
legalNeighbours terrain block = undefined
