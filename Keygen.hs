module Keygen 
    (
        generateKeyPair,
        PubKey,
        PrivKey,
        Prime
    ) where

import ModularArithmetics

import Control.Monad.Fix
import Control.Monad
import Data.Functor


type PubKey = (Integer, Integer)
type PrivKey = (Integer, Integer)
type Prime = Integer


fst3 :: (a, b, c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thd3 :: (a, b, c) -> c
thd3 (a,b,c) = c

boolFromIO :: IO Bool -> Bool
boolFromIO = boolFromIO

generateRandomPrime :: Int -> IO Prime
generateRandomPrime blockSize =
    do
        n <- generateNewNumber blockSize
        isprime <- millerRabinPrimality n
        if (isprime) then
            return n
        else 
            generateRandomPrime blockSize

-- Generates p q ane returns fixed exponent 65537
generateEPQ :: IO (Integer, Prime, Prime)
generateEPQ = 
    do
        let blockSize = 512 :: Int
            e = 65537 :: Integer
        p <- generateRandomPrime blockSize 
        q <- generateRandomPrime blockSize
        let phi = p*q
        if ((gcd e phi) == 1) then 
                return (e, p, q)
        else 
            generateEPQ

-- Generates key pair which are stored in files
generateKeyPair :: String -> String -> IO ()
generateKeyPair pubPath privPath =
    do putStrLn "------------------------------------------"
       putStrLn "----- Generating public/private keys -----"
       putStrLn "..........It may take some time..........."
       putStrLn "------------------------------------------"
       writeFile (pubPath) ""
       writeFile (privPath) ""
       expPrimes <- generateEPQ :: IO (Integer, Prime, Prime)
       let e = fst3 expPrimes
           p = snd3 expPrimes
           q = thd3 expPrimes
           n = p*q
           phi = (p-1)*(q-1)
           d = inverseMod e phi :: Integer
           resultPub = (e, n) :: PubKey
           resultPriv = (d, n) :: PrivKey
       writeFile (pubPath) (show resultPub)
       writeFile (privPath) (show resultPriv)
       putStrLn ("Key pair saved in '" ++ pubPath ++
                                 "' and '" ++ pubPath ++ "'")