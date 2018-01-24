module Vigenere(
    vigenereEncryptMapping,
    vigenereDecryptMapping,
    addElems,
    vigenereEncrypt,
    vigenereDecrypt
) where

vigenereEncryptMapping :: Char -> Char -> Char
vigenereEncryptMapping a b = toEnum ((((fromEnum a) + (fromEnum b) - 194) `mod` 26) + 97)

vigenereDecryptMapping :: Char -> Char -> Char
vigenereDecryptMapping a b = toEnum ((((fromEnum a) - (fromEnum b)) `mod` 26) + 97)

nthElemModulo :: Integer -> [a] -> a
nthElemModulo n xs = xs !! fromInteger (n `mod` toInteger (length xs)) 

addElems :: Integer -> [a] -> [a]
addElems n xs = loop xs where
     loop acc = if (length acc) > (fromInteger n)
            then acc
        else addElems n (xs++xs)


vigenereEncrypt :: String -> String -> String
vigenereEncrypt content key = zipWith vigenereEncryptMapping content ( addElems (toInteger $ length content) key )

vigenereDecrypt :: String -> String -> String
vigenereDecrypt content key = zipWith vigenereDecryptMapping content ( addElems (toInteger $ length content) key )