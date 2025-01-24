{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BangPatterns #-}

module Day20b (runme, runex) where

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
import AOCHelper (readInpByteSTring)
import qualified BSArray as BSA
import BSArray (BSArray)
import PosDir ( Pos(..), Loc, loc, rl, rr, (.->.), (.+.), cardinalDirections, mhdist )
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Map as M
import qualified Control.Parallel.Strategies as S
import qualified Data.Vector.Unboxed as V 

example :: ByteString
example =
  [r|###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
|]

runex :: RunMe
runex =
  runMeByteString
    "Day 20 - example"
    (return example)
    part1
    (Nothing)
    part2
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "-- Day 20: Race Condition / PAR + Vec --"
    (readInpByteSTring "day20.txt")
    part1
    (Just 1263)
    part2
    (Just 957831)


--
walk :: BSArray -> Loc -> Loc -> Loc -> [Loc]
walk bsa prev here endloc
            |  here == endloc = [here]
            |  otherwise = here : walk bsa here next endloc
            where next =
                      head
                      $ filter (\l -> l /= prev && BSA.lookup bsa l /= '#')
                      $ map (here .->.) cardinalDirections

part1 :: ByteString -> IO Integer
part1 s =  do
     let bsa = BSA.makeBSarray s
     let sp = fromMaybe (error "S does not exist") $ BSA.elemIndex bsa 'S'
     let ep = fromMaybe (error "E does not exist") $ BSA.elemIndex bsa 'E'
     let mymap = V.fromList (walk bsa sp sp ep)
     let !c = (map (numCheatsIx mymap 2 ) [0..((V.length mymap) -1)] )  `S.using` S.parListChunk 100 S.rseq
     return . toInteger . sum $ c

-- cheat gain = pathdistance of points - mhdist of points 
numCheatsIx :: V.Vector Loc-> Int -> Int  -> Int
numCheatsIx map cheattime ix =
    let ixtocheck = (ix + 100) -- start checking at least 100 steps away
        maxix = V.length (map) - 1 
    in if ixtocheck >= maxix then 0 else length . filter isValidCheat . takeWhile (<= maxix) $ (iterate nextIxToTest ixtocheck)
    where thispos =  map V.! ix
          dist i = mhdist thispos (map V.! i)
          nextIxToTest i = i + max (dist i - cheattime) 1  -- if we're 30 away, and cheattime = 20 then we can skip 10 steps ahead 
          isValidCheat i = let d = dist i in d <= cheattime && (i - ix - d) >= 100

part2 :: ByteString -> IO Integer
part2 s = do
     let bsa = BSA.makeBSarray s
     let sp = fromMaybe (error "S does not exist") $ BSA.elemIndex bsa 'S'
     let ep = fromMaybe (error "E does not exist") $ BSA.elemIndex bsa 'E'
     let mymap = V.fromList (walk bsa sp sp ep)
     let !c = (map (numCheatsIx mymap 20 ) [0..((V.length mymap) -1)] ) `S.using` S.parListChunk 100 S.rseq
     return . toInteger . sum $ c
