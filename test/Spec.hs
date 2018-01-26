module Main where

import Test.Hspec

import RandomPrimes
import Rsa
import Vigenere
import Text.Read
import System.Directory

main :: IO ()
main = hspec $ do
    vigenereTest
    primesTest
    rsaTest

vigenereTest :: Spec
vigenereTest = describe "Vigenere." $
        it "should return the same value" $ decoded `shouldBe` "test"
            where decoded = vigenereDecrypt (vigenereEncrypt "test" "key") "key"

primesTest :: Spec
primesTest = describe "Random Primes." $
    it "should return that number is prime" $ isPrime 7919 `shouldBe` True

rsaTest :: Spec
rsaTest = describe "Rsa" $
    it "should return the same message" $ "Test string" `shouldBe` decoded
        where 
            encrypted = rsaEncryptString (412091,557393) "Test string"
            decoded = rsaEncryptString (190499,557393) encrypted