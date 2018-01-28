module Main where

import Test.Hspec

import RandomPrimes
import Test.Quick

main :: IO ()
main = hspec $ do
    vigenereTest
    primesTest
    rsaTest
