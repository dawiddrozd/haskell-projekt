module Main where

import RandomPrimes

main :: IO ()
main = do
    (a,b) <- rndPrimes 42
    print a
    print b