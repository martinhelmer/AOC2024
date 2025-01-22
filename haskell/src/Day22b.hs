{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Day22b (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  decimal,
  endOfInput,
  endOfLine,
  isDigit,
  many1,
  parseOnly,
  skipSpace,
  skipWhile,
 )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, Pos, Dir, tp)
import qualified BSArray as BSA
import Data.Bits (xor, Bits (shiftL, shiftR))
import Data.Int (Int32, Int64)
import Data.Foldable (foldl')
import Data.Vector.Unboxed (Vector, (//))
import qualified Data.Vector.Unboxed as V
import Control.Seq (using)
import qualified Control.Parallel.Strategies as S
import Data.List.Split (chunksOf)
import Data.Word (Word16, Word8)
import Data.Array.Unboxed (accumArray,(!), Array)

example :: ByteString
example =
  [r|1
10
100
2024|]

runex :: RunMe
runex =
  runMeByteString
    "Day 22 - example"
    (return example)
    part1
    (Just 37327623)
    part2
    (Just 24)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 22: Monkey Market MAP ---"
    (readInpByteSTring "day22.txt")
    part1
    (Just 13004408787)
    part2
    (Just 1455)

type In = Int
---
mix' :: (In -> In) -> In -> In
mix' f a = (f a) `xor` a

prune :: In -> In
prune a = a `mod` 16777216

nxt :: In -> In
nxt = let l = prune . mix' (`shiftL` 11)  . prune . mix' (`shiftR` 5) . prune . mix' (`shiftL` 6 ) in l
{-# INLINE nxt #-} 

get1 :: [(In,Integer)] -> In -> In -> In -> In -> [In] -> [(In, Integer)]
get1 l a b c d (e:xs) | null xs = f:l
                      | otherwise = get1 (f:l) b c d e xs
                where f = ((b-a+9) * 19^3 + (c-b+9)*19^2 + (d-c+9)*19 + (e-d+9),fromIntegral e)
get1 _ _ _ _ _ _= undefined


get1' :: [In] -> [(In, Integer)]
get1' (a:b:c:d:e:xs) = get1 [] a b c d (e:xs)

mkmap :: In -> IntMap Integer
mkmap n = IM.fromListWith (flip const) . drop 3 . get4'' 0 0 $ (doN' n 2001)  -- 2001 take 2001 . iterate nxt
{-# INLINE mkmap #-} 


get4'' :: In   -- last key
          -> In -- last val 
          -> [In] 
          -> [(In,Integer)]
get4'' lk lv [] = []
get4'' lk lv (a:xs) = let s = (lk * 19 + ((a-lv) + 9)) `mod` 130321 in (s,fromIntegral a) : get4'' s a (xs)
{-# INLINE get4'' #-} 


doN :: (Eq t, Num t) => In -> t -> In
doN i 0 = i
doN i n = let nn = doN (nxt i) (n-1) in nn


part1 :: ByteString -> IO Integer
part1 s = do
    let !numbers = map (read . BS.unpack) $ BS.lines s::[In]
        !ss = (map (`doN` 2000) numbers)  `S.using` S.parListChunk 32 S.rdeepseq
    return . toInteger . sum $ ss

uv :: [IntMap Integer] -> IntMap Integer
uv = foldl' (IM.unionWith (+)) IM.empty

part2 :: ByteString -> IO Integer
part2 s = do
     let numbers = map (read . BS.unpack) $ BS.lines s::[In]
         maps = map mkmap numbers  `S.using` S.parListChunk (length numbers `div` 16 ) S.rseq
         total = uv maps 
     -- print (sum . map IM.size $ maps)
     return . toInteger $ maximum . IM.elems $ total

---
doN' :: In -> In -> [In]
doN' i 0 = []
doN' i n = (i `mod` 10) : doN' (nxt i) (n-1)
{-# INLINE doN' #-} 
