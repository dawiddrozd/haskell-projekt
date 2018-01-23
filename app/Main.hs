module Main where

import RandomPrimes

main :: IO ()
main = do
    (a,b) <- rndPrimes 20
    print a
    print b