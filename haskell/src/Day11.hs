{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day11 (runme, runex) where

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
    "--- Day 11: Plutonian Pebbles"
    (readInpByteSTring "day11.txt")
    part1
    (Just 199986)
    part2
    (Just 236804088748754)

---

blink1' :: Integral a => [(a, b)] -> [(a, b)]
blink1' [] = []
blink1' ((x,n):xs)  | x == 0 = (1,n) : blink1' xs
                    | x <= 9 = (x * 2024,n): blink1' xs
                    | x <= 99 = (x `div` 10,n):(x `mod` 10,n):blink1' xs
                    | x <= 999 = (x * 2024,n): blink1' xs
                    | x <= 9999 = (x `div` 100,n):(x `mod` 100,n):blink1' xs
                    | x <= 99999 = (x * 2024,n): blink1' xs
                    | x <= 999999 = (x `div` 1000,n):(x `mod` 1000,n):blink1' xs
                    | x <= 9999999 = (x * 2024,n): blink1' xs
                    | x <= 99999999 = (x `div` 10000,n):(x `mod` 10000,n):blink1' xs
                    | x <= 999999999 = (x * 2024,n): blink1' xs
                    | x <= 9999999999 = (x `div` 100000,n):(x `mod` 100000,n):blink1' xs
                    | x <= 99999999999 = (x * 2024,n): blink1' xs
                    | x <= 999999999999 = (x `div` 1000000,n):(x `mod` 1000000,n):blink1' xs
                    | otherwise = undefined

blink :: IM.IntMap Int -> IM.IntMap Int
blink  = IM.fromListWith (+) . blink1' . IM.toList

blinkTwice :: IM.IntMap Int -> IM.IntMap Int
blinkTwice  = IM.fromListWith (+) . blink1' . blink1' . IM.toList

-- ugly parse. 
parse :: ByteString -> [(Int,Int)]
parse = map (\t -> (fst t,1)) . mapMaybe BS.readInt . BS.splitWith (== ' ')
---

go :: Int -> [(IM.Key, Int)] -> Int
go n l = sum $  IM.elems $ if even n then m1 else blink m1
    where m1 = iterate blinkTwice (IM.fromList l) !! max 0 (n `div` 2)

part1 :: ByteString -> IO Integer
part1  =  return . toInteger . go 25 . parse

part2 :: ByteString -> IO Integer
part2  = return . toInteger . go 75 . parse
