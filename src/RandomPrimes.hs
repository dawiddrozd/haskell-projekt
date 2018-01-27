{-|
    Module      : RandomPrimes
    Description : Module used for generating random prime number
-}

module RandomPrimes
    (
        rndPrimes,
        isPrime
    ) where

import System.Random

-- | Function that check if given number is prime
isPrime :: 
    Integer -- ^ Number to check
    -> Bool -- ^ Return true if given number is prime
isPrime k = null [ x | x <- [2..isqrt k], k `mod` x  == 0]
        where isqrt = floor . sqrt . fromIntegral

-- | Function that generate random primes
rndPrimes :: 
    Int -- ^ bits of the number
    -> IO (Integer, Integer) -- ^ IO tuple of two different prime numbers
rndPrimes bits = do
    p <- rndPrime bits
    q <- rndPrime bits
    if p /= q then return (p, q) else rndPrimes bits

-- Function that generate one random prime number
rndPrime :: Int -> IO Integer
rndPrime bits = do
        x <- randomRIO (2^(bits - 1), 2^bits - 1)
        if isPrime x then return x else rndPrime bits