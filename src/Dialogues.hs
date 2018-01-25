{-#LANGUAGE ScopedTypeVariables#-}

module Dialogues(
    mainDialogue
) where

import RandomPrimes
import Rsa
import Encryption
import Control.Monad.Fix
import Vigenere
import Text.Read
import System.Directory

mainDialogue :: IO ()
mainDialogue = do
    putStrLn "What do you want to do?"
    putStrLn "[1] Encrypt the message."
    putStrLn "[2] Decrypt the message from the file."
    putStr "Your choice: "
    line <- getLine
    let maybeNum = readMaybe line :: Maybe Integer
    case maybeNum of 
        Just 1 -> encryptionDialog
        Just 2 -> decryptionDialog

encryptionDialog :: IO ()
encryptionDialog = putStrLn "Choose encryption algorithm: "
    >> putStrLn "[1] RSA Encryption"
    >> putStrLn "[2] Vigenere cipher"
    >> encryptionAlgorithmChooser


decryptionDialog :: IO ()
decryptionDialog = putStrLn "Choose encryption algorithm: "
    >> putStrLn "[1] RSA Encryption"
    >> putStrLn "[2] Vigenere cipher"
    >> decryptionAlgorithmChooser

encryptionAlgorithmChooser :: IO ()
encryptionAlgorithmChooser = fix $ \repeat -> do
    putStr "Your choice: "
    line <- getLine
    let maybeNum = readMaybe line :: Maybe Integer
    case maybeNum of 
        Just 1 -> rsaEncryption
        Just 2 -> vigenereCipherEncryption
        _ -> do
            putStrLn "Incorrect choice. You should choose 1 or 2."
            repeat

decryptionAlgorithmChooser :: IO ()
decryptionAlgorithmChooser = fix $ \repeat -> do
    putStr "Your choice: "
    line <- getLine
    let maybeNum = readMaybe line :: Maybe Integer
    case maybeNum of 
        Just 1 -> rsaDecryption
        Just 2 -> vigenereCipherDecryption
        _ -> do
            putStrLn "Incorrect choice. You should choose 1 or 2."
            repeat

rsaEncryption :: IO ()
rsaEncryption = do
    putStrLn "What do you want to encrypt? "
    msg <- getLine
    putStrLn "In which file do you want to keep your message?"
    fileName <- getLine
    primes <- rndPrimes 10
    keys <- uncurry publicAndPrivateKey primes
    let public = fst keys
    let private = snd keys
    putStrLn $ "public key : " ++ show public
    putStrLn $ "private key : " ++ show  private
    putStrLn "Starting RSA enryption..."
    let coded = rsaEncryptString public msg
    encryptRsaToFile coded fileName

vigenereCipherEncryption :: IO ()
vigenereCipherEncryption = do
    putStrLn "What do you want to encrypt? "
    msg <- getLine
    putStrLn "What is your key?"
    key <- getLine
    putStrLn "In which file do you want to keep your message?"
    fileName <- getLine
    putStrLn "Starting Vigenere encryption..."
    let coded = vigenereEncrypt msg key
    encryptVigenereToFile coded fileName


vigenereCipherDecryption :: IO ()
vigenereCipherDecryption = do
    putStrLn "Choose file"
    fileName <- getLine
    fileExist <- doesFileExist fileName
    if fileExist
        then do
            content <- readFile fileName
            putStrLn "What is your key?"
            key <- getLine
            putStrLn "It's your message:"
            putStrLn $ vigenereDecrypt content key

        else do
            putStrLn $ "File " ++ fileName ++ " does not exists!"

rsaDecryption :: IO ()
rsaDecryption = do
    putStrLn "Choose file"
    fileName <- getLine
    fileExist <- doesFileExist fileName
    if fileExist
        then do
            content <- readFile fileName
            putStrLn "Pass first number in private key"
            first :: Integer <- readLn
            putStrLn "Pass second number"
            second :: Integer <- readLn
            let key = (first, second)
            putStrLn "It's your message:"
            putStrLn $ rsaEncryptString key content
        else do
            putStrLn $ "File " ++ fileName ++ " does not exists!"

encryptRsaToFile :: String -> String -> IO ()
encryptRsaToFile encryptedMsg fileName = do
    writeFile fileName encryptedMsg
    putStrLn $ "Message encrypted using RSA algorithm in file: " ++ fileName

encryptVigenereToFile :: String -> String -> IO ()
encryptVigenereToFile encryptedMsg fileName = do
    writeFile fileName encryptedMsg
    putStrLn $ "Message encrypted in file using Vigenere algorithm in file: " ++ fileName
    
    