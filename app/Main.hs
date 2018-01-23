module Main where

import RandomPrimes
import Rsa
import Encryption

main :: IO ()
main = do
    primes <- rndPrimes 11
    let p = fst primes
    let q = snd primes
    keys <- publicAndPrivateKey p q
    let public = fst keys
    let private = snd keys
    let encoded = encrypt public 123
    print $ decrypt private encoded