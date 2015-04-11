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

os2ip :: ByteString -> Integer
os2ip = BS.foldl (\ a b -> (256 * a) + (fromIntegral b)) 0

i2osp :: Integer -> Int -> ByteString
i2osp x len = digits
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

chunkMessage :: Integer -> ByteString -> [ByteString]
chunkMessage n message = chunkify message size
    where
        size = getNextPossibleCharBlockSize n

-- executes encryption
encryptExec :: Integer -> Integer -> Integer -> Integer
encryptExec e n m = powerMod m e n

-- executes decryption
decryptExec :: Integer -> Integer -> Integer -> Integer
decryptExec d n c = powerMod c d n

encryptString :: Integer -> Integer -> ByteString -> [Integer]
encryptString e n message = 
        map (\x -> encryptExec e n (os2ip x)) (chunkMessage n message)


decryptString :: Integer -> Integer -> [Integer] -> ByteString
decryptString d n message =
    BS.concat (map (\x -> i2osp (decryptExec d n x) size) message)
    where
        size = getNextPossibleCharBlockSize n
