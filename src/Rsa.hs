{-|
    Module      : Rsa
    Description : Module with standard operations using Rsa Algorithm
-}

module Rsa (
    publicAndPrivateKey, rsaEncryptString
) where

import System.Random

-- Function that calculate the totient
getFi :: 
    Integer -- ^ First prime number
    -> Integer -- ^ Second prime number
    -> Integer -- ^ Totient
getFi p q = (p-1)*(q-1)

-- | Function that calculate the modulus for the public and private key
getN :: Integer -> Integer -> Integer
getN p q = p * q

-- | Function that generate public key exponent
getE :: 
    Integer -- ^ Prime number
    -> IO Integer -- ^ IO public key exponent
getE fi =  do
    acc <- randomRIO (2,fi-1)
    if (gcd acc fi == 1) then return acc
    else getE fi

-- | Implemetation of Extended Euclidean algorithm
gcdExt :: Integral b => b -> b -> (b, b, b)
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)
 
-- | Calculate private key exponent
getD :: Integer -> Integer -> Integer
getD a m = let (i, _, g) = gcdExt a m
             in mkPos i
  where mkPos x = if x < 0 then x + m else x

-- | Function that generate public and private key for given prime numbers
publicAndPrivateKey :: 
    Integer -- ^ First prime number
    -> Integer -- ^ Second prime number
    -> IO ((Integer,Integer),(Integer,Integer)) -- ^ Tuple of public and private key
publicAndPrivateKey p q = do
    e <- getE (getFi p q)
    let fi = getFi p q
    let d = getD e fi
    let n = getN p q
    let publicKey = (e, n)
    let privateKey = (d, n)
    return (publicKey, privateKey)

-- | Function performs encryption and decryption operations in RSA algorithm
-- encryption and decryption is done using the same function, 
-- we only change key to private or public
encrypt :: 
    (Integer, Integer) -- ^ Public/private key
    -> Integer -- ^ Encrypted/Decrypted integer
    -> Integer -- ^ Decrypted/Encrypted integer
encrypt (e, n) t = (t^e) `mod` n

-- | Function that perform RSA algorithm on String
rsaEncryptString :: 
    (Integer, Integer) -- ^ Public/private key
    -> String -- ^ Encrypted/Decrypted string
    -> String -- ^ Decrypted/Encrypted string
rsaEncryptString key msg = fmap (toEnum) [fromInteger $ encrypt key (toInteger $ fromEnum x) | x <- msg] :: [Char]