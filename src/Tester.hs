module Tester where

import Data.Set (Set, notMember, union, fromList, singleton)

import Block
import Bloxorz
import Levels
import Terrain

-- Helper variables for testing on REPL

pa = Pos (X 0) (Y 0)
pb = Pos (X 0) (Y 0)
block = makeBlock pa pb

l0 = buildLevel level0
l1 = buildLevel level1
l3 = buildLevel level3
l6 = buildLevel level6
li = buildLevel impossible
infl = Level pa (Pos (X 5) (Y 5)) (const True)
state0 = (block, [])
startSet = singleton block
