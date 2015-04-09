module Main ( main ) where

import Keygen
import Primitives
import System.Environment ( getArgs )


callKeygen :: [String] -> IO ()
callKeygen [pubPath, privPath] = do
    generateKeyPair pubPath privPath
callKeygen [] = do
    generateKeyPair "examples/pub.key" 
                            "examples/priv.key"
callKeygen _ = putStrLn ("ERROR: Wrong number" ++ 
                            " of arguments to Keygen")

callEncrypt :: [String] -> IO ()
callEncrypt [] = do
    encrypt "examples/pub.key" 
            "examples/message.txt" 
            "examples/cyphertext.txt"
callEncrypt [pubPath] = do
    encrypt pubPath
                "examples/message.txt" 
                "examples/cyphertext.txt"
callEncrypt _ = putStrLn ("ERROR: Wrong number of" ++
                                " arguments to Encrypt")

callDecrypt :: [String] -> IO ()
callDecrypt [] = do
    decrypt "examples/priv.key" 
            "examples/cyphertext.txt"
            "examples/decrypted.txt"
callDecrypt [privPath] = do
    decrypt privPath
                "examples/cyphertext.txt"
                "examples/decrypted.txt"
callDecrypt _ = putStrLn ("ERROR: Wrong number of " ++ 
                                "arguments to Decrypt")

callHelp :: [String] -> IO ()
callHelp _ = do
    putStrLn ("This program is the command line " ++ 
                          "interface to the RSA " ++
                         "encryption algorithm\n")
    putStrLn ("Usage: hsrsa COMMAND [PARAMS]\n")
    putStrLn ("Commands:")
    putStrLn ("   keygen <path-to-pub-key> " ++
                        "<path-to-priv-key>")
    putStrLn ("   encrypt <path-to-pub-key> " ++ 
                         "<input-path> <output-path>")
    putStrLn ("   decrypt <path-to-priv-key> " ++ 
                         "<input-path> <output-path>")

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("keygen", callKeygen)  
            , ("encrypt", callEncrypt)  
            , ("decrypt", callDecrypt)
            , ("help", callHelp) 
            ]  


main :: IO ()
main = do
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  