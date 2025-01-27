{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TupleSections #-}

module Day10 (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString)


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import qualified BSArray as BSA
import BSArray (makeBSarray, Index, BSArray)
import Data.Maybe (mapMaybe)
import PosDir ((.+.))
import Data.Containers.ListUtils (nubOrd)

example :: ByteString
example =
  [r|89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
|]

runex :: RunMe
runex =
  runMeByteString
    "Day 10 - example"
    (return example)
    part1
    (Just 36)
    part2
    (Just 81)

runme :: RunMe
runme =
  runMeByteString
    "-- Day 10: Hoof It ---"
    (readInpByteSTring "day10.txt")
    part1
    (Just 796)
    part2
    (Just 1942)

---
stepUp :: BSArray -> (Index, Char) -> [(Index, Char)]
stepUp grid (p,c) = mapMaybe ll locs
        where locs = map (p .+.) [(0,1), (0,-1), (1,0), (-1,0)]
              ll p' =
                case BSA.lookupMaybe grid p' of
                        Nothing -> Nothing
                        Just c' -> if c' == succ c then Just (p', c') else Nothing

peaks :: BSArray -> (Index, Char) -> [Index]
peaks grid (p, c) | c == '9' = [p]
                  | otherwise = concatMap (peaks grid ) $ stepUp grid (p, c)


go :: ByteString -> ([Index] -> [Index]) -> IO Integer
go s op = do
    let grid = makeBSarray s
    let trailheads = map (, '0') (BSA.elemIndices '0' grid)
    return . toInteger . sum . map  (length . op .  peaks grid) $ trailheads


part1 :: ByteString -> IO Integer
part1 s = go s nubOrd

part2 :: ByteString -> IO Integer
part2 s = go s id