{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Day25 (runme, runex) where

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


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, splitOnBs)
import qualified BSArray as BSA
import Data.Foldable (foldl')
import Data.List (transpose)
import Data.Bifunctor (bimap)
import Control.Monad (join)

example :: ByteString
example =
  [r|#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
|]

runex :: RunMe
runex =
  runMeByteString
    "Day 25 - example"
    (return example)
    part1
    (Nothing)
    part2
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 25: Code Chronicle ---"
    (readInpByteSTring "day25.txt")
    part1
    (Just 3264)
    part2
    (Nothing)

---
fits :: [Int] -> [Int] -> Bool
fits [] [] = True
fits (x:xs) (y:ys) = x+y <= 5 && fits xs ys

pairs :: [b] -> [a] -> [(a, b)]
pairs l1 = concatMap (\a -> map (\b -> (a,b))  l1)

pairs' a b = map (\[x,y] -> (x,y)) $ sequence [a,b]

number :: [ByteString] -> [Int]
number = map (BS.length . BS.filter (=='#')) . BS.transpose . tail

part1 :: ByteString -> IO Integer
part1 s = do
  return . toInteger 
         . length 
         . filter (uncurry fits) 
         . (uncurry pairs')
         . join bimap (map number)
         . foldl' (\(la, ka) l -> if head l == "#####" then (l:la,ka) else (la,(reverse l):ka)) ([],[]) 
         . map BS.lines 
         $ splitOnBs "\n\n" (BS.init s)
  
part2 :: ByteString -> IO Integer
part2 s = return 0
