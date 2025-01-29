{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Day22c (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import Data.Bits (xor, Bits (shiftL, shiftR))
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Control.Monad.ST (runST, ST)
import Control.Monad (when)
import qualified Control.Parallel.Strategies as S
import Data.Word (Word16)


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
    (part1 . numbers')
    (Just 37327623)
    (part2 . numbers')
    (Just 24)

runme :: RunMe
runme =
  runMeByteString
    "-- Day 22c: MonkeyMarket (MVec)"
    (readInpByteSTring "day22.txt")
    (part1 . numbers')
    (Just 13004408787)
    (part2 . numbers') 
    (Just 1455)

type In = Int
type Val = Word16
---

nxt :: In -> In
nxt = let !l = prune . mix' (`shiftL` 11)
             . prune . mix' (`shiftR` 5)
             . prune . mix' (`shiftL` 6 ) in l
      where prune a = a `mod` 16777216
            mix' f a = (f a) `xor` a

doN :: In -> In -> In
doN i 0 = i
doN i n = doN (nxt i) (n-1)

numbers' :: ByteString -> [In]
numbers' s = map (read . BS.unpack) $ BS.lines s::[In]

part1 :: [In] -> IO Integer
part1 numbers = do
    let !ss = map (`doN` 2000) numbers  `S.using` S.parListChunk 32 S.rdeepseq
    return . toInteger . sum $ ss


part2 :: [In] -> IO Integer
part2 numbers = do
     let vectors= runST (mkv numbers )
     return . toInteger $ V.maximum vectors

step :: ((Int,Int),Int)  ->  ((Int,Int),Int)
step ((prevkey, prevv),prevseed) =
    let newseed = nxt prevseed
        newval =  newseed `mod` 10
        nextkey = (prevkey * 19 +  (newval - prevv + 9)) `mod` 130321
    in ((nextkey, newval),newseed)

mkv :: [Int] -> ST s (V.Vector Val)
mkv seeds = do
    v <- MV.replicate (19^(4::Int)-1) (0::Val)
    visited <- MV.replicate (19*19*19*19-1) (0::Int)
    mapM_ (\(ix, s) ->fillme visited ix v (1997::Int) (step . step . step $ ((0,0),s))) $ zip [1..] seeds
    V.freeze v

fillme :: V.MVector (MV.PrimState (ST s)) Int
          -> Int
          -> V.MVector (MV.PrimState (ST s)) Val
          -> Int
          -> ((Int, Int), Int) -> ST s ()
fillme visited ix v n s@((key,val),_)
           | n == 0 = pure ()
           | otherwise =  do
                vix <- MV.unsafeRead visited key
                when (vix /= ix) 
                    (do 
                    MV.unsafeWrite visited key ix
                    MV.unsafeModify v (\a -> a + fromIntegral val) key  
                    pure ())
                fillme visited ix v (n-1) (step s)
{-# INLINE fillme #-}
