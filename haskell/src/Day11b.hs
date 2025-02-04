{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day11b (runme, runex) where

import GHC.Exts
import Text.RawString.QQ
import qualified Data.IntMap.Strict as IM
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
import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, Pos, Dir, tp)
import qualified BSArray as BSA
import Data.Maybe (mapMaybe)
import qualified GHC.Integer.Logarithms as L
import qualified Data.MemoTrie as MT

example :: ByteString
example =
  [r|125 17
  |]

example2 :: ByteString
example2 = "0 1 10 99 999\n"

runex :: RunMe
runex =
  runMeByteString
    "Day 11 - example"
    (return example)
    part1
    (Just 55312)
    part2
    (Just 65601038650482)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 11: Plutonian Pebbles <memo> ---"
    (readInpByteSTring "day11.txt")
    part1
    (Just 199986)
    part2
    (Just 236804088748754)

---

blink :: Int -> Int -> Int
blink = MT.memo2 blink'

blink' :: Int -> Int  -> Int
blink' 0 _ = 1
blink' depth 0 = blink (depth -1) 1
blink' depth n= case ll `quotRem` (2::Int) of
            (w,0) -> let (r,l) = n `quotRem` (10 ^ w) 
                    in if depth == 1 then 2 else blink (depth -1) r + blink (depth- 1) l
            _ -> if depth == 1 then 1 else blink  (depth -1) (n * 2024)
    where len':: Int -> Int 
          len' n =  (1 +  I# (L.integerLogBase# 10 (toInteger n))) :: Int --1 +  floor (logBase 10 (fromIntegral n))::Int
          ll = len' n 

-- ugly parse. 
parse :: ByteString -> [Int]
parse = map fst . mapMaybe BS.readInt . BS.splitWith (== ' ')
---

go :: Int -> [Int] -> Integer
go n l = toInteger . sum $ map (blink n)  l

part1 :: ByteString -> IO Integer
part1  = return  . go 25 . parse

part2 :: ByteString -> IO Integer
part2  = return . go 75 . parse
