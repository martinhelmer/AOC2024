{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Day22 (runme, runex) where

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
import Data.Vector.Unboxed (Vector, (//), (!))
import qualified Data.Vector.Unboxed.Mutable as MV 
import qualified Data.Vector.Unboxed as V 
import Data.Vector (forM_)


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
    "--- Day 22: Monkey Market ---"
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
nxt = let !l = prune . mix' (`shiftL` 11)  . prune . mix' (`shiftR` 5) . prune . mix' (`shiftL` 6 ) in l 


get4 :: Num t => t -> t -> t -> t -> [t] -> [((t, t, t, t), t)]
get4 a b c d (e:xs) | null xs = [f]
                    | otherwise = f:get4 b c d e xs
                where f = ((b-a, c-b, d-c, e-d),e)
get4 _ _ _ _ _= undefined


get1 :: [(In,In)] -> In -> In -> In -> In -> [In] -> [(In, In)]
get1 l a b c d (e:xs) | null xs = f:l
                      | otherwise = get1 (f:l) b c d e xs
                where f = ((b-a+9) * 19^3 + (c-b+9)*19^2 + (d-c+9)*19 + (e-d+9),e)
get1 _ _ _ _ _ _= undefined


get1' :: [In] -> [(In, In)]
get1' (a:b:c:d:e:xs) = get1 [] a b c d (e:xs)

mkmap :: In -> HashMap In In
mkmap n = M.fromListWith (\ _ x -> x) . get1' $ (doN' n 2001)  -- 2001 take 2001 . iterate nxt

mkvector :: In -> Vector In
mkvector n = let nv =  V.replicate (19^4) (0::In) in nv  // ( (get1' $ (doN' n 2001)))


-- get4'' :: In -> [(In, In)] -> [(In,In)]
-- get4'' n [] = []
-- get4'' n (a:xs) = let s = n * 19 + (fst a + 9) `mod` 130321 in (s, snd a) : get4'' s (xs)


doN :: (Eq t, Num t) => In -> t -> In
doN i 0 = i
doN i n = let nn = doN (nxt i) (n-1) in nn 

doN' :: (Eq t, Num t) => In -> t -> [In]
doN' i 0 = []
doN' i n = (i `mod` 10) : doN' (nxt i) (n-1)

part1 :: ByteString -> IO Integer
part1 s = do
    let numbers = map (read . BS.unpack) $ BS.lines s::[In]
    return . toInteger . sum $ (map (`doN` 2000) numbers)


uv' :: [Vector In] -> Vector In
uv' = foldl' (\v1 v2 -> V.generate (19^4) (\i -> v1 ! i + v2 ! i)) (V.replicate (19^4) 0) 

part2 :: ByteString -> IO Integer
part2 s = do
     let numbers = map (read . BS.unpack) $ BS.lines s::[In]
         vectors = map mkvector numbers
         total = uv' vectors
    --  print (V.length . V.filter (==0) $ total)
    --  print (V.length . V.filter (>100) $ total)
     return . toInteger $ V.maximum total


step :: ((Int,Int),Int)  ->  ((Int,Int),Int)
step ((prevkey, prevv),prevseed) = ((prevkey, prevv),prevseed)

mkv :: (MV.PrimState (V.MVector t) ~ t, MV.PrimMonad (V.MVector t)) => Int -> V.MVector t Int
mkv seed = do 
    v <- MV.replicate (19*19*19*19) (0::Int)
    fillme v 2000 ((0,0),seed)
    V.freeze v 

fillme :: (Eq t, Num t, MV.PrimMonad f) => V.MVector (MV.PrimState f) Int -> t -> ((Int, Int), Int) -> f ()
fillme v n s@((key,val),_) | n == 0 = pure ()
           | otherwise =  do
                _ <- MV.write v key val
                fillme v (n-1) (step s)
