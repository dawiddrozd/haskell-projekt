module HUnit where 

import Test.Hspec
import Test.HUnit

import Vigenere
import Rsa
import RandomPrimes

vigenereTest :: Test
vigenereTest = 
    TestCase $ assertEqual "Should return \"test\"" "test" (vigenereDecrypt (vigenereEncrypt "test" "key") "key")

primesTest :: Test
primesTest =
    TestCase $ assertEqual "should return that number is prime" True $ isPrime 2147483647


addElemsTest :: Test
addElemsTest = TestCase $ assertEqual "Should return \"abcabcbacabc\"" "abcabcabcabc" $ addElems 8 "abc"   

rsaTest :: Test
rsaTest = 
    TestCase $ assertEqual "should return the same message" "Test string" decoded
        where 
            encrypted = rsaEncryptString (412091,557393) "Test string"
            decoded = rsaEncryptString (190499,557393) encrypted


nthElemModuloTest :: Test
nthElemModuloTest = TestCase $ assertEqual "should return 'u' " 'u' $ nthElemModulo 10 "tyui"            

unitTests = TestList [vigenereTest, addElemsTest, primesTest, nthElemModuloTest, rsaTest]