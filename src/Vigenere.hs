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

-- | Function that mapps maps char with key char to encrypted char 
vigenereEncryptMapping :: 
    Char    -- ^ char to encrypt
    -> Char -- ^ key char
    -> Char -- ^ encrypted char
vigenereEncryptMapping a b = toEnum ((fromEnum a) + (fromEnum b))

-- | Function that decrypts char using key char
vigenereDecryptMapping :: 
    Char    -- ^ char to decrypt 
    -> Char -- ^ key char
    -> Char -- ^ decrypted char
vigenereDecryptMapping a b = toEnum ((fromEnum a) - (fromEnum b))

-- | Function that return element of index n mod (length xs)
nthElemModulo :: 
    Integer -- n 
    -> [a]  -- list
    -> a    -- element of index n mod (length xs)
nthElemModulo n xs = xs !! fromInteger (n `mod` toInteger (length xs)) 

-- | Function that doubles length of list untill it's length is bigger than n
addElems :: 
    Integer -- ^ Number n
    -> [a] -- ^ list for replication
    -> [a] -- ^ Replicated list
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