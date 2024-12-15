{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day06 (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString)

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import PosDir (Pos(..), Loc, Dir(..), rr, step, loc, pos2int, loc2int)

import qualified BSArray as BSA
import BSArray (BSArray)
import Data.Maybe (fromJust, isJust)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.IntSet as IS 

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

type Visited = IS.IntSet

vismember :: Pos -> IS.IntSet -> Bool
vismember p s = (pos2int p) `IS.member` s 

dobigstep :: BSArray -> Maybe Loc -> Pos -> Maybe Pos
dobigstep bsa = dostep bsa True 

dosmallstep :: BSArray -> Maybe Loc -> Pos -> Maybe Pos
dosmallstep bsa = dostep bsa False 

dostep :: BSA.BSArray -> Bool ->  Maybe Loc -> Pos -> Maybe Pos
dostep bsa many blocked p@(Pos l d) = if Just l' == blocked then  rr' else  case stpc of
                        Nothing -> Nothing
                        Just '#' -> rr'
                        _ -> if many then dostep bsa many blocked stp else Just stp
    where stp@(Pos l' _) = step p
          stpc = BSA.lookupMaybe bsa l'
          rr' =  Just (Pos l (rr d))

part1 :: ByteString -> IO Integer
part1 s  = do
    let bsa = BSA.makeBSarray s
    let sp = Pos (fromJust $ BSA.elemIndex bsa '^') NORTH
    return (toInteger . length . nubOrd . map (\(Just (Pos p _)) -> p) . takeWhile (isJust) $ iterate (>>= (dosmallstep bsa Nothing )) (Just sp) )

hasLoop :: BSArray -> Visited -> Pos -> Loc  -> Bool
hasLoop bsa v sp l =  hasLoop' v (Just sp)
    where
        hasLoop' _ Nothing = False
        hasLoop' v' (Just p') = p' `vismember` v' || hasLoop' ( (pos2int p') `IS.insert` v' ) (dobigstep bsa (Just l) p')


martin :: BSArray -> [Loc] -> IS.IntSet -> Visited-> Pos -> [Loc]
martin bsa accumblocks vloc visited currentpos =
    case  dosmallstep bsa  Nothing currentpos of
            Nothing -> accumblocks
            (Just nextp) ->
                martin
                    bsa
                    (let l = loc nextp in if not (IS.member (loc2int l) vloc) && hasLoop bsa visited currentpos l then l:accumblocks else accumblocks)
                    ((loc2int . loc $ currentpos) `IS.insert` vloc)
                    ((pos2int currentpos) `IS.insert` visited)
                    nextp

part2 :: ByteString -> IO Integer
part2 s = do
    let bsa = BSA.makeBSarray s
    let sp = Pos (fromJust $ BSA.elemIndex bsa '^') NORTH

    return ( toInteger . length  $ martin bsa [] IS.empty IS.empty sp )
