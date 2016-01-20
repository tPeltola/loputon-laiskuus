module Tester where

import Data.Set (Set, notMember, union, fromList, singleton)

import Block
import Bloxorz
import Levels
import Terrain

pa = Pos 0 0
pb = Pos 0 0
block = makeBlock pa pb

l0 = buildLevel level0
l1 = buildLevel level1
l3 = buildLevel level3
l6 = buildLevel level6
li = buildLevel impossible
infl = Level pa (Pos 5 5) (const True)
state0 = (block, [])
startSet = singleton block
