module ModularArithmetics 
    (
        inverseMod,
        euclid,
        powerMod,
        generateNumberInRange,
        generateNewNumber,
        millerRabinPrimality
    ) where

import OpenSSL.BN

-- Modular multiplicative inverse
inverseMod :: Integer -> Integer -> Integer
inverseMod e phi =
  (x + phi) `mod` phi
  where
    (x, y) = euclid e phi

-- Extended euclidean algorithm
euclid :: Integer -> Integer -> (Integer, Integer)
euclid 0 _ = (0,1)
euclid _ 0 = (1,0)
euclid e n = (t, s-q*t)
    where
      (q, r) = quotRem e n
      (s, t) = euclid n r

-- Modular exponentiation
powerMod :: Integer -> Integer -> Integer -> Integer
powerMod b e m = powerModExec b (toBin e) m 1

 -- Modular exponentiation execution
powerModExec :: Integer -> [Integer] -> Integer -> Integer -> Integer
powerModExec _ [] _ c = c
powerModExec b (1:e) m c = powerModExec b e m ((c^2 `mod ` m)*b `mod` m)
powerModExec b (0:e) m c = powerModExec b e m (c^2 `mod` m)

-- Convert Integer to an Integer list which represents original Integer in binary
toBin :: Integer -> [Integer]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise = toBin (n `div` 2) ++ [1]

-- Generates random number in specified range
generateNumberInRange :: Integer -> Integer -> IO Integer
generateNumberInRange mn mx = randIntegerUptoNMinusOneSuchThat (\a -> a > mn) mx

generateNewNumber :: Int -> IO Integer
generateNewNumber n = 
    let mn = (2::Integer)^(n-1)
        mx = (2::Integer)^n
    in randIntegerUptoNMinusOneSuchThat (\a -> a > mn) mx

-- Decomposition function
decomp :: Integer -> (Integer,Integer)
decomp n = decomp' (0,n) where
decomp' = until (odd.snd) (\ (m,n) -> (m+1,div n 2))

-- Main function that implements Miller-Rabin test
-- for arbitrary precision
primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = let
        (r,s) = decomp (n-1)
        f = \ x -> takeWhile (/= 1) (map (\ j -> powerMod x (2^j*s) n) [0..r])
    in
        do
            a <- (generateNumberInRange 1 (n-1))
            if powerMod a (n-1) n /= 1
                then return False
                else
                    if powerMod a s n /= 1 && last (f a) /= (n-1)
                        then return False
                        else primeMR (k-1) n

-- Wrapper with recommended precision k=40
millerRabinPrimality :: Integer -> IO Bool
millerRabinPrimality n = primeMR 40 n