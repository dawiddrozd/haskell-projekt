module Main where

import Test.HUnit

import RandomPrimes
import Quick
import HUnit

--main :: IO ()
main = do
    runTestTT HUnit.unitTests
    Quick.runTests