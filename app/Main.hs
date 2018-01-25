module Main where

import RandomPrimes
import Rsa
import Encryption
import Control.Monad.Fix
import Vigenere
import Text.Read
import System.Directory

main :: IO ()
main = do
    putStrLn "What do you want to do?"
    putStrLn "[1] Encrypt the message."
    putStrLn "[2] Decrypt the message from the file."
    putStr "Your choice: "
    line <- getLine
    let maybeNum = readMaybe line :: Maybe Integer
    case maybeNum of 
        Just 1 -> dialog
        Just 2 -> do
            putStr "Enter the filename: " >> getLine >>= 
                \fileName -> case fileName of
                    "rsa" -> decryptRsaFromFile fileName
                    "vig" -> decryptVigenerFromFile fileName
        _ -> putStrLn "Incorrect choice. Try again" >> main

dialog :: IO ()
dialog = putStrLn "Choose encryption algorithm: "
    >> putStrLn "[1] RSA Encryption"
    >> putStrLn "[2] Vigenere cipher"
    >> algorithmChooser

algorithmChooser :: IO ()
algorithmChooser = fix $ \repeat -> do
    putStr "Your choice: "
    line <- getLine
    let maybeNum = readMaybe line :: Maybe Integer
    case maybeNum of 
        Just 1 -> rsaEncryption
        Just 2 -> vigenereCipher
        _ -> do
            putStrLn "Incorrect choice. You should choose 1 or 2."
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
    putStrLn "Starting RSA enryption..."
    let coded = encryptString public msg
    encryptRsaToFile private coded

vigenereCipher :: IO ()
vigenereCipher = do
    putStr "What do you want to encrypt? "
    msg <- getLine
    let key = "kacper"
    putStrLn "Starting Vigenere encryption..."
    let coded = vigenereEncrypt msg key
    encryptVigenereToFile key coded

encryptRsaToFile :: (Integer,Integer) -> String -> IO ()
encryptRsaToFile key encryptedMsg = do
    let fileName = "rsa"
    writeFile fileName $ show key
    appendFile fileName $ "\n" ++ encryptedMsg
    putStrLn $ "Message encrypted using RSA algorithm in file: " ++ fileName

encryptVigenereToFile :: String -> String -> IO ()
encryptVigenereToFile key encryptedMsg = do
    let fileName = "vig"
    writeFile fileName key
    appendFile fileName $ "\n" ++ encryptedMsg
    putStrLn $ "Message encrypted in file using Vigenere algorithm in file: " ++ fileName

decryptRsaFromFile :: String -> IO ()
decryptRsaFromFile fileName = do
    fileExist <- doesFileExist fileName
    if not fileExist then putStrLn $ "File " ++ fileName ++ " does not exists!"
    else do
        content <- readFile fileName
        let linesOfFile = lines content
        let private = read $ linesOfFile !! 0
        let coded = linesOfFile !! 1
        putStr "Encrypted message: "
        print $ encryptString private coded

decryptVigenerFromFile :: String -> IO ()
decryptVigenerFromFile fileName = do
    fileExist <- doesFileExist fileName
    if not fileExist then putStrLn $ "File " ++ fileName ++ " does not exists!"
        else do
            content <- readFile fileName
            let linesOfFile = lines content
            let key = linesOfFile !! 0
            let coded = linesOfFile !! 1
            putStr "Encrypted message: "
            print $ vigenereDecrypt coded key
            putStrLn "No i sie zjebalo cos "