module Main ( main ) where

import Keygen
import Primitives
import System.Environment ( getArgs )
import qualified Data.ByteString.Lazy as BS


decrypt :: String -> String -> String -> IO ()
decrypt privKeyFileName cypherFileName decryptedFileName =
    do 
       stringFileContents <- readFile (privKeyFileName)
       let priv = read stringFileContents :: PrivKey
           d = fst priv
           n = snd priv
       cipher <- readFile (cypherFileName)
       let c = read cipher :: [Integer]
       writeFile (decryptedFileName) ""
       BS.writeFile (decryptedFileName)  (decryptString d n c)
       putStr "Decrypted text stored in: "
       putStrLn (decryptedFileName)

encrypt :: String -> String -> String -> IO ()
encrypt pubKeyFileName messageFileName cypherTextName =
    do 
       stringFileContents <- readFile (pubKeyFileName)
       let pub = read stringFileContents :: PubKey
           e = fst pub
           n = snd pub
       message <- BS.readFile(messageFileName)
       writeFile (cypherTextName) ""
       writeFile (cypherTextName) 
                    (show (encryptString e n message))
       putStrLn ("Cyphertext stored in: " ++ cypherTextName)

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