module Main where

import UnitTests
import Test.HUnit

main :: IO ()
main = do
        runTestTT tests
        return ()
