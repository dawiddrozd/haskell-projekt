{-# LANGUAGE TemplateHaskell #-}

module Quick where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)


import Rsa
import Encryption
import RandomPrimes
import Vigenere


genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf' genSafeChar


newtype SafeString = SafeString { unwrapSafeString :: String }
    deriving Show

instance Arbitrary SafeString where
    arbitrary = SafeString <$> genSafeString

listOf' :: Gen a -> Gen [a]
listOf' gen = do
  k <- choose (1,50)
  vectorOf k gen

checkRSA :: SafeString -> Property
checkRSA (SafeString str) = monadicIO $ do
    primes <- run $ rndPrimes 8
    keys <- run $ uncurry publicAndPrivateKey primes
    let public = fst keys
    let private = snd keys
    let coded = rsaEncryptString public str
    let decoded = rsaEncryptString private coded
    assert $ str == decoded

checkVigenere :: SafeString -> SafeString -> Property
checkVigenere (SafeString msg) (SafeString key) = monadicIO $ do
    let coded = vigenereEncrypt msg key
    let decoded = vigenereDecrypt coded key
    assert $ msg == decoded

runTests = do
    quickCheck checkRSA 
    quickCheck checkVigenere


    