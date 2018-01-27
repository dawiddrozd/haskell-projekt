{-#LANGUAGE ScopedTypeVariables#-}

{-|
    Module      : Encryption
    Description : Module that privides support for encryption and decryption operations
-}

module Encryption (
    rsaEncryption,rsaDecryption,
    vigenereCipherEncryption,vigenereCipherDecryption,
    encryptRsaToFile,encryptVigenereToFile
) where

import RandomPrimes
import Rsa
import Vigenere
import System.Directory

-- | Function provides standard encryption operations using RSA
-- writes the encrypted string given from the command line
-- to the selected file in the current directory
-- show public and private key
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

-- | Function provides standard encryption operations using Vigenere cipher
-- writes the encrypted string given from the command line
-- to the selected file in the current directory
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

-- | Function provides standard decryption operation using vigenere Cipher
-- requests the key used for encryption and the file name with the encrypted message
-- shows a message when the file with the given name does not exist
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

-- | Function provides decryption IO operations using RSA
-- requests the private key used for encryption and file name with message
-- shows warning when the file with the given name does not exist
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


-- | Function that support writing to file (RSA)
encryptRsaToFile :: 
    String -- ^ String with encrypted message
    -> String -- ^ File name
    -> IO () -- ^ Message saved to file
encryptRsaToFile encryptedMsg fileName = do
    writeFile fileName encryptedMsg
    putStrLn $ "Message encrypted using RSA algorithm in file: " ++ fileName

-- | Function that support writing to file (Vigenere)
encryptVigenereToFile :: 
    String -- ^ String with encrypted message
    -> String -- ^ File name
    -> IO () -- -- ^ Message saved to file
encryptVigenereToFile encryptedMsg fileName = do
    writeFile fileName encryptedMsg
    putStrLn $ "Message encrypted in file using Vigenere algorithm in file: " ++ fileName