
module Lib
    (
        randomInteger, maxExponent, maxDivider, divide, checkPrime
    ) where

import System.Random


min' = 2^120
max' = 2^150

randomInteger :: IO Integer
randomInteger = randomRIO (min', max')

maxExponent :: Integer -> Integer
maxExponent x = last [b | b <- [0..150] , x `mod` (2^b) == 0]

maxDivider :: Integer -> Integer
maxDivider n = quot n (2 ^  (maxExponent n))



divide :: Integer -> Integer -> Integer
divide a b = quot a b


checkPrime :: Integer -> Integer -> IO Bool
checkPrime n =  do
            a <- randomInteger(1,n)
            if(mod a^d n /= 1)
               then return (checkPrime2 a d n)
            else
                return False
            where
                d = maxDivider n
            
--checkPrime2 :: IO Bool
checkPrime2 a b c = return True
