{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

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
nxt = prune . mix' (`shiftL` 11)  . prune . mix' (`shiftR` 5) . prune . mix' (`shiftL` 6 )


get4 a b c d (e:xs) | null xs = [f a b c d e]
                    | otherwise = (f a b c d e):get4 b c d e xs
                where f a b c d e = ((b-a, c-b, d-c, e-d),e)
get4 _ _ _ _ _= undefined

get1 a b c d (e:xs) | null xs = [f a b c d e]
                    | otherwise = (f a b c d e):get1 b c d e xs
                where f a b c d e = ((b-a) * 19^3 + (c-b)*19^2 + (d-c)*19 + (e-d),e)
get1 _ _ _ _ _= undefined

get1' (a:b:c:d:e:xs) = get1 a b c d (e:xs)

get4' (a:b:c:d:e:xs) = get4 a b c d (e:xs)


mkmap n = IM.fromListWith (\ _ x -> x) . get1' $ (doN' n 2001)  -- 2001 take 2001 . iterate nxt

mkmap' :: In -> HashMap In In
mkmap' n = let r = map (`mod` 10) . take 2001 $ iterate nxt n in
        M.fromListWith (\ _ x -> x) . drop 3 . get4'' 0 $ zipWith (\a b -> (a-b,a)) (tail r) r


get4'' :: In -> [(In, In)] -> [(In,In)]
get4'' n [] = []
get4'' n (a:xs) = let s = n * 19 + (fst a + 9) `mod` 130321 in (s, snd a) : get4'' s (xs)

doN i 0 = i
doN i n = doN (nxt i) (n-1)

doN' i 0 = []
doN' i n = (i `mod` 10) : doN' (nxt i) (n-1)

part1 :: ByteString -> IO Integer
part1 s = do
    let numbers = map (read . BS.unpack) $ BS.lines s::[In]
    return . toInteger . sum $ (map (`doN` 2000) numbers)


uv = foldl (IM.unionWith (+)) IM.empty

part2 :: ByteString -> IO Integer
part2 s = do
     let numbers = map (read . BS.unpack) $ BS.lines s::[In]
         maps = map mkmap numbers
         total = uv maps
     return . toInteger . maximum . IM.elems $ total
