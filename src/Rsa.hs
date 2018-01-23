module Rsa (
    publicAndPrivateKey
) where

import RandomPrimes
import System.Random

getFi :: Integer -> Integer -> Integer
getFi p q = (p-1)*(q-1)

getN :: Integer -> Integer -> Integer
getN p q = p * q


getE :: Integer -> IO Integer
getE fi =  do
    acc <- randomRIO (2,fi-1)
    if (gcd acc fi == 1) then return acc
    else getE fi


getD' :: Integer -> Integer -> Integer
getD' e fi = head [x | x <- [1..fi-1] , (b*x) `mod` fi == 1]
        where b = e `mod` fi

gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)
 
-- Given a and m, return Just x such that ax = 1 mod m.  If there is no such x
-- return Nothing.
getD :: Integer -> Integer -> Integer
getD a m = let (i, _, g) = gcdExt a m
             in mkPos i
  where mkPos x = if x < 0 then x + m else x

publicAndPrivateKey :: Integer -> Integer -> IO ((Integer,Integer),(Integer,Integer))
publicAndPrivateKey p q = do
    e <- getE (getFi p q)
    let fi = getFi p q
    let d = getD e fi
    let n = getN p q
    let publicKey = (e, n)
    let privateKey = (d, n)
    return (publicKey, privateKey)