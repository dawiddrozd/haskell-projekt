module Encryption(
    encrypt,
    decrypt
) where


encrypt :: (Integer, Integer) -> Integer -> Integer
encrypt (e, n) t = (t^e) `mod` n


decrypt :: (Integer, Integer) -> Integer -> Integer
decrypt (d, n) c = (c^d) `mod` n
