module Test.Quick where

import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Rsa
import Encryption
import RandomPrimes

main = do
    putStrLn "RSA testing"
    quickCheck checkRSA

checkRSA :: String -> Property
checkRSA msg = monadicIO $ do
    primes <- run $ rndPrimes 10
    keys <- run $ uncurry publicAndPrivateKey primes
    let public = fst keys
    let coded = rsaEncryptString public msg
    let decoded = rsaEncryptString public coded
    assert $ coded == decoded