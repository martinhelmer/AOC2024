{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}


module Day11 (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString)
import Data.List ( sort )
import Data.Bifunctor (second)

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import qualified BSArray as BSG
import BSArray (makeBSarray)

example :: ByteString
example =
  [r|...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....|]

runex :: RunMe
runex =
  runMeByteString
    "Day x - example"
    (return example)
    part1
    (Just 374)
    part2
    (Just 82000210)

runme :: RunMe
runme =
  runMeByteString
    "Day 11: Cosmic Expansion"
    (readInpByteSTring "day11.txt")
    part1
    (Just 10292708)
    part2
    (Just 790194712336)

---
diffs :: Num c => [c] -> [c]
diffs l = zipWith (-) (tail l ) l

pairwiseCombinations :: [Int] -> [(Int,Int)]
pairwiseCombinations []  = []
pairwiseCombinations (x:xs)  = let q = map ((,) x) xs in q <> pairwiseCombinations xs


sumup :: Int -> [Int] -> Int
sumup dist coords = sum
          . map (\(a,b) -> b - a)
          . pairwiseCombinations
          . scanl (\curr next -> if next <= 1 then curr+next else curr + (next -1)*dist+1) (head coords)
          $ diffs coords

part1 :: ByteString -> IO Integer
part1 s = do
  let (rowcoords, colcoords) = second sort . unzip . BSG.elemIndices '#' . makeBSarray $ s
  return . toInteger $ (sumup 2 rowcoords + sumup 2 colcoords)

part2 :: ByteString -> IO Integer
part2 s = do
  let (rowcoords, colcoords) = second sort . unzip . BSG.elemIndices '#' . makeBSarray $ s
  return . toInteger $ (sumup 1000000 rowcoords + sumup 1000000 colcoords)