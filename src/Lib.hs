
module Lib
    (
        randomInteger, maxExponent, maxDivider, divide, checkPrime
    ) where

import System.Random

min' = 2^120
max' = 2^150

randomInteger :: Integer -> Integer -> IO Integer
randomInteger a b = randomRIO (a, b)

maxExponent :: Integer -> Integer
maxExponent x = last [b | b <- [0..150] , x `mod` (2^b) == 0]

maxDivider :: Integer -> Integer
maxDivider n = quot n (2 ^ (maxExponent n))

divide :: Integer -> Integer -> Integer
divide a b = quot a b

checkPrime :: Integer -> IO Bool
checkPrime n =  do
            a <- randomInteger 2 (n-2)
            x <- return $ mod (a^d) n
            if(x == 1 || x == (n-1))
               then return $ checkPrime2 a d n
            else
                return False
            where
                d = maxDivider n
            
checkPrime2 :: Integer -> Integer -> Integer -> Bool
checkPrime2 a b c = True
