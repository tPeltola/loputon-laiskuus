module Block where

import Control.Exception (assert)
import Terrain
import Prelude hiding (Right, Left)

-- In Bloxorz, we can move left, right, Up or down.
-- These moves are encoded as case objects.
data Move = Up | Down | Left | Right deriving Show

-- A block is represented by the position of the two cubes that
-- it consists of.
data Block = B { p1 :: Pos, p2 :: Pos } deriving (Ord, Eq, Show)

-- We make sure that `b1` is lexicographically
-- smaller than `b2`.
makeBlock :: Pos -> Pos -> Block
makeBlock p1 p2 = assert (x p1 <= x p2 && y p1 <= y p2) $ B p1 p2

type BlockState = (Level, Block)

-- The position obtained by changing the `y` coordiante by `d`
dy :: Pos -> Int -> Pos
dy pos i = pos { y = y pos + i }

-- The position obtained by changing the `x` coordiante by `d`
dx :: Pos -> Int -> Pos
dx pos i = pos { x = x pos + i }

-- Returns a block where the `y` coordinates of `b1` and `b2` are
-- changed by `d1` and `d2`, respectively.
blockDy :: Block -> Int -> Int -> Block
blockDy b d1 d2 = makeBlock (dy (p1 b) d1) (dy (p2 b) d2)

-- Returns a block where the `x` coordinates of `b1` and `b2` are
-- changed by `d1` and `d2`, respectively.
blockDx :: Block -> Int -> Int -> Block
blockDx b d1 d2 = makeBlock (dx (p1 b) d1) (dx (p2 b) d2)

-- This function returns the block at the start position of
-- the game.
startBlock :: Level -> Block
startBlock l = undefined

-- Returns `true` if the block is standing.
standing :: Block -> Bool
standing b = undefined

vertical :: Block -> Bool
vertical b = (x.p1) b == (x.p2) b

left :: Block -> Block
left block
  | standing block   = blockDy block (-2) (-1)
  | vertical block   = blockDy block (-1) (-2)
  | otherwise        = blockDy block (-1) (-1)

right :: Block -> Block
right block
  | standing block   = blockDy block 1 2
  | vertical block   = blockDy block 2 1
  | otherwise        = blockDy block 1 1

up :: Block -> Block
up block
  | standing block   = blockDx block (-2) (-1)
  | vertical block   = blockDx block (-1) (-1)
  | otherwise        = blockDx block (-1) (-2)

down :: Block -> Block
down block
  | standing block   = blockDx block 1 2
  | vertical block   = blockDx block 1 1
  | otherwise        = blockDx block 2 1

-- Returns the list of blocks that can be obtained by moving
-- the current block, together with the corresponding move.
neighbours :: Block -> [(Block, Move)]
neighbours b = undefined

-- Returns `true` if the block is entirely inside the terrain.
isLegal :: Block -> (Pos -> Bool) -> Bool
isLegal b terrf = undefined

-- Returns the list of positions reachable from the current block
-- which are inside the terrain.
legalNeighbours :: Block -> (Pos -> Bool) -> [(Block, Move)]
legalNeighbours block terrf = undefined
