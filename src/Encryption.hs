module Encryption(
    encrypt,
    decrypt,
    szyfr',
    rsaEncryptString
) where

--function modular_pow(base, exponent, modulus)
--    if modulus = 1 then return 0 
--    c := 1
--    for e_prime = 1 to exponent 
--       c := (c * base) mod modulus
--    return c


encrypt :: (Integer, Integer) -> Integer -> Integer
encrypt (e, n) t = (t^e) `mod` n

szyfr' :: (Integer, Integer) -> Integer -> Integer
szyfr' (exponent,modulus) base = 
    if modulus == 1 then 0
    else loop 0 0 where
        loop x acc = if x == exponent
            then
                loop (x+1) (acc + ((acc * base) `mod` modulus))
            else acc
        

decrypt :: (Integer, Integer) -> Integer -> Integer
decrypt (d, n) c = (c^d) `mod` n


rsaEncryptString :: (Integer, Integer) -> String -> String
rsaEncryptString key msg = fmap (toEnum) [fromInteger $ encrypt key (toInteger $ fromEnum x) | x <- msg] :: [Char]