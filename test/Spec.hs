module Main where

import Test.Tasty
import Test.Tasty.HUnit

import RandomPrimes
import Rsa
import Encryption
import Control.Monad.Fix
import Vigenere
import Text.Read
import System.Directory

main :: IO ()
main = defaultMain (testGroup "Our Library Tests" [test1])

test1 = testCase "Vigenere test" (assertEqual "Should return \"test\" " "test" $ vigenereDecrypt (vigenereEncrypt "test" "dupa") "dupa")

--rsaTest = do
--    primes <- rndPrimes 10
--    keys <- uncurry publicAndPrivateKey primes
--    let public = fst keys
--    let private = snd keys
--    let coded = rsaEncryptString public "test"
--    let decoded = rsaEncryptString private coded
--    putStrLn decoded

-- chuj kurwa nie dziala. nie wiem da sie porownywac IOIOIOIOIOIO


--test2 = testCase "RSA test" (assertEqual "Should return " (putStrLn "test") rsaTest)