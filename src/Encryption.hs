module Encryption(
    encrypt,
    decrypt
) where

--function modular_pow(base, exponent, modulus)
--    if modulus = 1 then return 0 
--    c := 1
--    for e_prime = 1 to exponent 
--       c := (c * base) mod modulus
--    return c


encrypt :: (Integer, Integer) -> Integer -> Integer
encrypt (e, n) t = (t^e) `mod` n

decrypt' :: (Integer, Integer) -> Integer -> Integer
decrypt' (exponent,modulus) base = 
    if modulus == 1 then 0
    else foldl $ (\acc _ -> mod $ (acc*base) modulus) 1 [1..exponent]
        

decrypt :: (Integer, Integer) -> Integer -> Integer
decrypt (d, n) c = (c^d) `mod` n
