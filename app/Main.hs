module Main where

import RandomPrimes
import Rsa
import Encryption
import Control.Monad.Fix
import Vigenere

main :: IO ()
main = dialog

dialog :: IO ()
dialog = putStrLn "Choose encryption algorithm: "
    >> putStrLn "[1] RSA Encryption"
    >> putStrLn "[2] Vigenere cipher"
    >> algorithmChooser

algorithmChooser :: IO ()
algorithmChooser = fix $ \repeat -> do
    putStr "Your choice: "
    num <- read <$> getLine
    case num of 
        1 -> putStrLn "Starting RSA enryption..." >> rsaEncryption
        2 -> putStrLn "Starting Vigenere encryption..." >> vigenereCipher
        _ -> do
            putStrLn "You should choose 1 or 2"
            repeat

rsaEncryption :: IO ()
rsaEncryption = do
    putStr "What do you want to encrypt? "
    msg <- getLine
    primes <- rndPrimes 10
    let p = fst primes
    let q = snd primes
    keys <- publicAndPrivateKey p q
    let public = fst keys
    let private = snd keys
    let coded = encryptString public msg
    print $ encryptString private coded

vigenereCipher :: IO ()
vigenereCipher = do
    putStr "What do you want to encrypt? "
    msg <- getLine
    putStrLn "Kacper tu masz zaszyfrować"