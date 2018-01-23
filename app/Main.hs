module Main where

import RandomPrimes
import Rsa
import Encryption

main :: IO ()
main = do
    primes <- rndPrimes 40
    let p = fst primes
    let q = snd primes
    keys <- publicAndPrivateKey p q
    let public = fst keys
    let private = snd keys
    print $ snd keys
    print encrypt 