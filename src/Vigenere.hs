{-|
    Module      : Vigenere
    Description : Module with standard operations using Vigenere Cipher
-}

module Vigenere(
    vigenereEncryptMapping,
    vigenereDecryptMapping,
    addElems,
    nthElemModulo,
    vigenereEncrypt,
    vigenereDecrypt
) where


vigenereEncryptMapping :: Char -> Char -> Char
vigenereEncryptMapping a b = toEnum ((fromEnum a) + (fromEnum b))

vigenereDecryptMapping :: Char -> Char -> Char
vigenereDecryptMapping a b = toEnum ((fromEnum a) - (fromEnum b))

nthElemModulo :: Integer -> [a] -> a
nthElemModulo n xs = xs !! fromInteger (n `mod` toInteger (length xs)) 

-- | function that replicates string untill it's length is bigger than n
addElems :: 
    Integer -- ^ Number n
    -> [a] -- ^ Array for replication
    -> [a] -- ^ Replicated array
addElems n xs = loop xs where
     loop acc = if (length acc) > (fromInteger n)
            then acc
        else addElems n (xs++xs)

-- | Function that encrypt given string using Vigenere algorithm
vigenereEncrypt :: 
    String -- ^ Content to encrypt
    -> String -- ^ Key used for encryption
    -> String -- ^ Encrypted message
vigenereEncrypt content key = zipWith vigenereEncryptMapping content ( addElems (toInteger $ length content) key )

-- | Function that decrypt given string using Vigenere algorithm
vigenereDecrypt :: 
    String -- ^ Message encrypted with Vigenere Cipher
    -> String -- ^ Key used for encryption
    -> String -- ^ Decrypted message
vigenereDecrypt content key = zipWith vigenereDecryptMapping content ( addElems (toInteger $ length content) key )