
module Keygen 
    (
        generateKeyPair,
        PubKey,
        PrivKey,
        Prime
    ) where

import ModularArithmetics

import Control.Monad.Fix
--import Control.Conditional
import Control.Monad
import Data.Functor

-- # type definitions
type PubKey = (Integer, Integer)
type PrivKey = (Integer, Integer)
type Prime = Integer

-- # private functions

-- define a three tuple
fst3 :: (a, b, c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thd3 :: (a, b, c) -> c
thd3 (a,b,c) = c

-- Some useless thing
boolFromIO :: IO Bool -> Bool
boolFromIO = boolFromIO


-- Interaction to get fitting primes and exponent
-- Control.Monad.Fix idea from StackOverflow: http://stackoverflow.com/a/13301611
enterExpPrimes :: IO (Integer, Prime, Prime)
enterExpPrimes =
    fix $ \again -> do
        putStrLn "Enter exponent (leave blank for default [65537])"
        exp <- getLine
        let e
              | exp == "" = 65537 :: Integer
              | otherwise = read exp :: Integer
        putStrLn "Enter first prime: "
        prime <- getLine
        let p = read prime :: Prime
        putStrLn "Enter second prime: "
        prime <- getLine
        let q = read prime :: Prime
            phi = (p-1)*(q-1)
        do 
            let isPrimeP = millerRabinPrimality p
                isPrimeQ = millerRabinPrimality q
            if ((gcd e phi) == 1 && boolFromIO isPrimeP  && boolFromIO isPrimeQ) then 
                return (e, p, q)
            else
               do putStrLn "ERROR: Exponent and primes don't match the requirements [(gcd e phi]) <> 1]"
                  again

generateRandomPrime :: Int -> IO Prime
generateRandomPrime blockSize =
    do
        n <- generateNewNumber blockSize
        isprime <- millerRabinPrimality n
        if (isprime) then
            return n
        else 
            generateRandomPrime blockSize


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


-- # public functions
-- Interaction to generate key pair which are stored in pub.key/priv.key
generateKeyPair :: IO ()
generateKeyPair =
    do putStrLn "---------------------------------------------------------------------------"
       putStrLn "-------------------------- Key generation started -------------------------"
       putStrLn "---------------------------------------------------------------------------"
       writeFile ("pub.key") ""
       writeFile ("priv.key") ""
       expPrimes <- generateEPQ :: IO (Integer, Prime, Prime)
       let e = fst3 expPrimes
           p = snd3 expPrimes
           q = thd3 expPrimes
           n = p*q
           phi = (p-1)*(q-1)
           d = inverseMod e phi :: Integer
           resultPub = (e, n) :: PubKey
           resultPriv = (d, n) :: PrivKey
       writeFile ("pub.key") (show resultPub)
       writeFile ("priv.key") (show resultPriv)
       putStrLn ("Key pair saved in pub.key and priv.key")