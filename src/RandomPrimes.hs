module RandomPrimes
    (
        rndPrimes,
        isPrime,
    ) where

import System.Random

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime k = null [ x | x <- [2..isqrt k], k `mod` x  == 0]

rndPrimes :: Int -> IO (Integer, Integer)
rndPrimes bits = do
    p <- rndPrime bits
    q <- rndPrime bits
    if p /= q then return (p, q) else rndPrimes bits

rndPrime :: Int -> IO Integer
rndPrime bits = do
        x <- randomRIO (2^(bits - 1), 2^bits - 1)
        if isPrime x then return x else rndPrime bits
