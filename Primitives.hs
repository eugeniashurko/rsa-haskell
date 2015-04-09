module Primitives 
    (
        encrypt,
        decrypt
    ) where

import Keygen
import ModularArithmetics
import Data.Char
import Data.Bits

-- executes encryption
encryptExec :: Integer -> Integer -> Integer -> Integer
encryptExec e n m = powerMod m e n

-- executes decryption
decryptExec :: Integer -> Integer -> Integer -> Integer
decryptExec d n c = powerMod c d n


-- main function to decrypt strings
decryptString :: Integer -> Integer -> [Integer] -> [Char]
decryptString d n cs
  | (getNextPossibleCharBlockSize n) == 0 = [' ']
  | otherwise = decryptBlocks d n cs


-- main function to encrypt strings
encryptString :: Integer -> Integer -> [Char] -> [Integer]
encryptString e n ms
  | getNextPossibleCharBlockSize n == 0 = [-1]
  | otherwise = encryptBlocks e n 
        (getMessageBlocks ms (getNextPossibleCharBlockSize n))

-- bs = block list
encryptBlocks :: Integer -> Integer -> [Integer] -> [Integer]
encryptBlocks e n bs
  | (length bs) == 1 = [encryptExec e n (head bs)]
  | otherwise = [encryptExec e n (head bs)] ++ 
                                (encryptBlocks e n (tail bs))


-- build list of message blocks, b = block size
getMessageBlocks :: String -> Int -> [Integer]
getMessageBlocks m b
  | (length m) <= b = [fromIntegral (charBlockToIntBlock m 0)]
  | otherwise = [fromIntegral (charBlockToIntBlock (take b m) 0)] 
                            ++ (getMessageBlocks (drop b m) b)

-- cb = char block, e = exponent (start with 0)
charBlockToIntBlock :: [Char] -> Int -> Int
charBlockToIntBlock cb e
  | (length cb) == 1 = (ord (head cb)) * (256^e)
  | otherwise = ((ord (head cb)) * (256^e)) + 
                        charBlockToIntBlock (tail cb) (e+1)

-- bs = blocklist
decryptBlocks :: Integer -> Integer -> [Integer] -> [Char]
decryptBlocks d n bs
  | (length bs) == 1 = intBlockToCharBlock 
                 (fromIntegral (decryptExec d n (head bs)))
  | otherwise = intBlockToCharBlock 
            (fromIntegral (decryptExec d n (head bs))) ++ 
                                (decryptBlocks d n (tail bs))

  -- ib = int block, m = modulo, b = block size (chars)
intBlockToCharBlock :: Int-> [Char]
intBlockToCharBlock 0 = []
intBlockToCharBlock ib = [(chr (mod ib 256))] ++ 
                            intBlockToCharBlock (shiftR ib 8)


-- if n < m -> RSA not possible, 
-- returns number of chars, not actual size
getNextPossibleCharBlockSize :: (Integral b, Num a, Ord a) 
                                                    => a -> b
getNextPossibleCharBlockSize n = 
                            snd (getNextSmallerPowerOfN 256 n)

-- returns last power of n which is still smaller than x
getNextSmallerPowerOfN :: (Integral b, Num t, Ord t) 
                                            => t -> t -> (t, b)
getNextSmallerPowerOfN n x = getNextSmallerPowerOfNExec n x 1

-- execute getNextSmallerPowerOfN
getNextSmallerPowerOfNExec :: (Integral b, Num t, Ord t) => 
                                        t -> t -> b -> (t, b)
getNextSmallerPowerOfNExec n x e
  | x > (n^e) = getNextSmallerPowerOfNExec n x (e+1)
  | x == (n^e) = (n^e,e)
  | otherwise = (n^(e-1),(e-1))

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
       writeFile (decryptedFileName) (decryptString d n c)
       putStr "Decrypted text stored in: "
       putStrLn (decryptedFileName)


-- interaction for encryption process
encrypt :: String -> String -> String -> IO ()
encrypt pubKeyFileName messageFileName cypherTextName =
    do 
       stringFileContents <- readFile (pubKeyFileName)
       let pub = read stringFileContents :: PubKey
           e = fst pub
           n = snd pub
       message <- readFile (messageFileName)
       writeFile (cypherTextName) ""
       writeFile (cypherTextName) 
                    (show (encryptString e n message))
       putStrLn ("Cyphertext stored in: " ++ cypherTextName)