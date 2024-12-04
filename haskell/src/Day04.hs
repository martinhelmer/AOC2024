{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day04 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
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
import Data.Maybe (catMaybes, mapMaybe)

example :: ByteString
example =
  [r|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|]

runex :: RunMe
runex =
  runMeByteString
    "Day x - example"
    (return example)
    part1
    (Just 18 )
    part2
    (Just 9)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 4: Ceres Search ---"
    (readInpByteSTring "day04.txt")
    part1
    (Just 2462)
    part2
    (Just 1877)

---
dirs :: (Num a,  Eq a, Enum a )  =>  [(a, a)]
dirs = [(x,y) | x <- [-1..1] , y<- [-1..1], (x,y) /= (0,0)]

pl :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
pl (a,b) (c,d) = (a+c,b+d)

tk bs p = mapMaybe l dirs
    where l d' = sequence $ map (BSA.lookupMaybe bs) ( take 3 $ iterate (pl d') (pl d' p))

tk2 bs p = l [(-1, -1), (1,1), (1,-1), (-1,1)]
    where l d' = sequence $ map (BSA.lookupMaybe bs) (map (pl p) d')


fit Nothing = False 
fit (Just s) = (s =="MSMS") || ( s == "MSSM") || (s == "SMSM") || (s == "SMMS")

part1 :: ByteString -> IO Integer
part1 s = do
    let b = BSA.makeBSarray s
    return  ( toInteger $ length $  concatMap (filter (== "MAS") . tk b) (BSA.elemIndices 'X' b) )

part2 :: ByteString -> IO Integer
part2 s = do 
    let b = BSA.makeBSarray s
    let l = filter (fit) $ map (tk2 b) (BSA.elemIndices 'A' b)
    return  ( toInteger $ length $  l )
