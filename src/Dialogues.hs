{-#LANGUAGE ScopedTypeVariables#-}

{-|
    Module      : Dialogues
    Description : Module that privides support for IO operation
-}

module Dialogues(
    mainDialogue
) where

import Control.Monad.Fix
import Text.Read

import Encryption

-- | Main function used for choose encryption/decryption
mainDialogue :: IO ()
mainDialogue = do
    putStrLn "What do you want to do?"
    putStrLn "[1] Encrypt the message."
    putStrLn "[2] Decrypt the message from the file."
    fix $ \repeat -> do
        putStr "Your choice: "
        line <- getLine
        let maybeNum = readMaybe line :: Maybe Integer
        case maybeNum of 
            Just 1 -> encryptionDialog
            Just 2 -> decryptionDialog
            _ -> do
                putStrLn "Incorrect choice. You should choose 1 or 2."
                repeat

-- | Function that print possible encryption methods for encryption choice
encryptionDialog :: IO ()
encryptionDialog = putStrLn "Choose encryption algorithm: "
    >> putStrLn "[1] RSA Encryption"
    >> putStrLn "[2] Vigenere cipher"
    >> encryptionAlgorithmChooser

-- | Function that print possible decryption methods for decryption choice
decryptionDialog :: IO ()
decryptionDialog = putStrLn "Choose decryption algorithm: "
    >> putStrLn "[1] RSA Encryption"
    >> putStrLn "[2] Vigenere cipher"
    >> decryptionAlgorithmChooser

-- | Function used for choose encryption algorithm
-- provides error handling when given wrong data
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

-- | Function used for choose decryption algorithm
-- provides error handling when given wrong data
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