module Primitives 
    (
        encryptString,
        decryptString
    ) where

import Keygen
import ModularArithmetics

import Data.Char
import Data.Int
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS


-- Get size of blocks less than modulo
getNextPossibleCharBlockSize :: (Integral b, Num a, Ord a) 
                                                    => a -> b
getNextPossibleCharBlockSize n = 
                            snd (getNextSmallerPowerOfN 256 n)

getNextSmallerPowerOfN :: (Integral b, Num t, Ord t) 
                                            => t -> t -> (t, b)
getNextSmallerPowerOfN n x = getNextSmallerPowerOfNExec n x 1

getNextSmallerPowerOfNExec :: (Integral b, Num t, Ord t) => 
                                        t -> t -> b -> (t, b)
getNextSmallerPowerOfNExec n x e
  | x > (n^e) = getNextSmallerPowerOfNExec n x (e+1)
  | x == (n^e) = (n^e,e)
  | otherwise = (n^(e-1),(e-1))

-- Converts ByteString to Integer
os2ip :: ByteString -> Integer
os2ip = BS.foldl (\ a b -> (256 * a) + (fromIntegral b)) 0

-- Converts Integer to ByteString
i2osp :: Integer -> ByteString
i2osp x = digits
    where
      digitize 0  = Nothing
      digitize v  = let (q, r) = divMod v 256
                    in Just(fromIntegral r, q)
      digits      = BS.reverse (BS.unfoldr digitize x)    

chunkify :: ByteString -> Int64 -> [ByteString]
chunkify bs size
  | BS.length bs == 0 = []
  | otherwise         = let (start, end) = BS.splitAt size bs
                        in start : chunkify end size

-- Chunk message into blocks of size less than modulo
chunkMessage :: Integer -> ByteString -> [ByteString]
chunkMessage n message = chunkify message size
    where
        size = getNextPossibleCharBlockSize n

encryptExec :: Integer -> Integer -> Integer -> Integer
encryptExec e n m = powerMod m e n

decryptExec :: Integer -> Integer -> Integer -> Integer
decryptExec d n c = powerMod c d n

encryptString :: Integer -> Integer -> ByteString -> [Integer]
encryptString e n message = 
        map (\x -> encryptExec e n (os2ip x)) (chunkMessage n message)

decryptString :: Integer -> Integer -> [Integer] -> ByteString
decryptString d n message =
    BS.concat (map (\x -> i2osp (decryptExec d n x)) message)

