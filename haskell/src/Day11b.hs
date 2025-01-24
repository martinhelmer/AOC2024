{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE MagicHash #-}

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

q :: Int -> String 
q n | n == 2  = "A"
q _ = "B"

blink1 :: Int -> [Int]
blink1 0 = [1] 
blink1 n = case l `quotRem` 2 of 
            (w,0) -> let (r,l) = n `quotRem` (10 ^ w) in [r,l]
            _ -> [n * 2024]
    where l = 1 + floor (logBase 10 (fromIntegral n))


blink1' ::[(Int, Integer)] -> [(Int, Integer)]
blink1' [] = []
blink1' ((x,n):xs)  | x == 0 = (1,n) : blink1' xs
                    | even l = (x `div` 10 ^ (l `div` 2),n):(x `mod` 10 ^ (l `div` 2),n):blink1' xs
                    | otherwise =  (x * 2024,n): blink1' xs
        where len':: Int -> Int 
              len' n =  (1 +  I# (L.integerLogBase# 10 (toInteger n))) :: Int --1 +  floor (logBase 10 (fromIntegral n))::Int
              l = len' x 
 

blink :: IM.IntMap Integer -> IM.IntMap Integer
blink  = IM.fromListWith (+) . blink1' . IM.toList

blinkTwice :: IM.IntMap Integer -> IM.IntMap Integer
blinkTwice  = IM.fromListWith (+) . blink1' . blink1' . IM.toList

-- ugly parse. 
parse :: ByteString -> [(Int,Integer)]
parse = map (\t -> (fst t,1)) . mapMaybe BS.readInt . BS.splitWith (== ' ')
---

go :: Int -> [(IM.Key, Integer)] -> Integer
go n l = sum $  IM.elems $ if even n then m1 else blink m1
    where m1 = iterate blinkTwice (IM.fromList l) !! max 0 (n `div` 2)

part1 :: ByteString -> IO Integer
part1  = return . toInteger . go 25 . parse

part2 :: ByteString -> IO Integer
part2  = return . toInteger . go 75 . parse
