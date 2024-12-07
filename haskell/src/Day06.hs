{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Day06 (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString, singleton)
import qualified Data.ByteString.Char8 as BS

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import PosDir (Pos(..), Loc, Dir(..), rr, step, loc)

import qualified BSArray as BSA
import BSArray (BSArray)
import Data.Maybe (fromJust, isJust, catMaybes, mapMaybe, isNothing)
import Data.Bifunctor(second)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Set as S
import qualified Data.HashSet as HS

example :: ByteString
example =
  [r|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...|]

runex :: RunMe
runex =
  runMeByteString
    "Day 06 - example"
    (return example)
    part1
    (Just 41)
    part2
    (Just 6)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 6: Guard Gallivant ---"
    (readInpByteSTring "day06.txt")
    part1
    (Just 5162)
    part2
    (Just 1909)

---
dostep :: BSA.BSArray -> Maybe Loc -> Pos -> Maybe Pos
dostep bsa blocked p@(Pos l d) = if Just l' == blocked then  dostep bsa blocked (Pos l (rr d)) else  case stpc of
                        Nothing -> Nothing
                        Just '#' -> dostep bsa blocked (Pos l (rr d))

                        _ -> Just stp
    where stp@(Pos l' _) = step p
          stpc = BSA.lookupMaybe bsa l'
          rr' =  Just (Pos l (rr d))

part1 :: ByteString -> IO Integer
part1 s  = do
    let bsa = BSA.makeBSarray s
    let sp = Pos (fromJust $ BSA.elemIndex bsa '^') NORTH
    return (toInteger . length . nubOrd . map (\(Just (Pos p d)) -> p) . takeWhile (isJust) $ iterate (>>= (dostep bsa Nothing )) (Just sp) )

type Visited = HS.HashSet Pos


hl2 :: BSArray -> Visited -> Pos -> Loc  -> Bool
hl2 bsa v sp l =  hasLoop' v (Just sp)
    where
        hasLoop' _ Nothing = False
        hasLoop' v' (Just p') = p' `HS.member` v' || hasLoop' (HS.insert p' v') (dostep bsa (Just l) p')



martin :: BSArray -> [Loc] -> (HS.HashSet Loc) -> Visited-> Pos -> [Loc]
martin bsa accumblocks vloc visited currentpos =
    case  dostep bsa Nothing currentpos of
            Nothing -> accumblocks
            (Just nextp) ->
                martin
                    bsa
                    (let l = loc nextp in if not (HS.member l vloc) && hasloop l then l:accumblocks else accumblocks)
                    ((loc currentpos) `HS.insert` vloc)
                    (currentpos `HS.insert` visited)
                    nextp

    where hasloop = hl2 bsa visited currentpos

part2 :: ByteString -> IO Integer
part2 s = do
    let bsa = BSA.makeBSarray s
    let sp = Pos (fromJust $ BSA.elemIndex bsa '^') NORTH

    return ( toInteger . length  $ martin bsa [] HS.empty HS.empty sp )
