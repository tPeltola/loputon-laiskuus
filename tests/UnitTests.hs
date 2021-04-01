module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Block
import Bloxorz
import Levels
import Terrain

import Prelude hiding (Right, Left)
import Data.Set (fromList)

-- To run the tests use `cabal test` command (or `cabal new-test` depending your cabal version)

main :: IO ()
main = defaultMain $ hUnitTestToTests tests

tests = TestList
  [ TestLabel "Terrain function for level 1" test1a
  , TestLabel "Terrain function for level 1" test1b
  , TestLabel "FindChar for level 1" test2
  , TestLabel "Optional solution for level 1" test3
  , TestLabel "Optional solution length for level 1" test4
  , TestLabel "Optional solution length for an infinite level" test5
  , TestLabel "Finding neighbors for level 1" test6
  , TestLabel "Finding new neighbors for level 1" test7
  ]

test1a = TestCase (assertBool "is terrain at 0,0" (terrain levelVector (newpos 0 0)))
test1b = TestCase (assertBool "is not terrain at 4,11" (not $ terrain levelVector (newpos 4 11)))

levelVector = toTerrain level1

test2 = TestCase (assertEqual "start == 1,1" (newpos 1 1) (start level))
  where level = buildLevel level1

test3 =
    TestCase (assertEqual "optimal solution for level 1" (makeBlock g g) (solve (makeBlock s s) moves))
  where s = start level
        g = goal level
        level = buildLevel level1
        moves = solution level

test4 =
    TestCase (assertEqual "optimal solution length for level 1" (length level1solution) (length moves))
  where level = buildLevel level1
        moves = solution level

test5 =
  TestCase (assertEqual "infinite level solution length" 8 (length (solution infiniteLevel)))

test6 =
  TestCase (assertEqual
    "Neighbor history matches"
    [(makeBlock (newpos 1 2) (newpos 1 3), [Right,Left,Up]), (makeBlock (newpos 2 1) (newpos 3 1), [Down,Left,Up])]
    (neighboursWithHistory level [Left, Up] (makeBlock (newpos 1 1) (newpos 1 1)))
  )
  where level = buildLevel level1

test7 =
  TestCase (assertEqual
    "Explored neigbors are ignored"
    [(makeBlock (newpos 2 1) (newpos 3 1), [Down,Left,Up])]
    (newNeighbours
      (fromList [makeBlock (newpos 1 2) (newpos 1 3), makeBlock (newpos 1 1) (newpos 1 1)])
      [(makeBlock (newpos 1 2) (newpos 1 3), [Right,Left,Up]), (makeBlock (newpos 2 1) (newpos 3 1), [Down,Left,Up])]
      )
  )


solve :: Block -> [Move] -> Block
solve start moves = foldl follow start moves
  where follow block Left  = left block
        follow block Right = right block
        follow block Up    = up block
        follow block Down  = down block

level1solution = [Right, Right, Down, Right, Right, Right, Down]

infiniteLevel = Level (newpos 0 0) (newpos 5 5) (const True)

newpos x y = Pos (X x) (Y y)
