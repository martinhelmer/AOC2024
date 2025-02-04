{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day20 (runme, runex) where

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
    "--- Day 20: Race Condition / v1 / PAR---"
    (readInpByteSTring "day20.txt")
    part1
    (Just 1263)
    part2
    (Just 957831)


---
parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s
--
walk :: BSArray -> Loc -> Loc -> Loc -> [Loc]
walk bsa prev here endloc
            |  here == endloc = [here]
            |  otherwise = here : walk bsa here next endloc
            where next =
                      head
                      $ filter (\l -> l /= prev && BSA.lookup bsa l /= '#')
                      $ map (here .->.) cardinalDirections


circle :: Int -> [(Int, Int)]
circle r = [(x,y) | x <- [-r..r] , y <- [-r..r] , let d = (abs x + abs y) in (d >= 2) && (d <= r) ]

cheats :: Int -> M.Map Loc Int -> Loc -> [Int]
cheats d m l = let !ll = mapMaybe (cheat m l . (l .+.)) (circle d) in ll

cheat m here d = let dst = (fromMaybe 0 ((-) <$> M.lookup d m <*> M.lookup here m) - mhdist here d) in if dst > 0 then Just dst else Nothing

-- 1269 too high 
part1 :: ByteString -> IO Integer
part1 s =  do
     let bsa = BSA.makeBSarray s
     let sp = fromMaybe (error "S does not exist") $ BSA.elemIndex bsa 'S'
     let ep = fromMaybe (error "E does not exist") $ BSA.elemIndex bsa 'E'
     let mymap = M.fromList $ zip  (walk bsa sp sp ep) [0..]
     let mycheats = concatMap (cheats 2 mymap) (M.keys mymap)
     return (toInteger . length . filter (>=100) $ mycheats )

part2 :: ByteString -> IO Integer
part2 s = do
     let bsa = BSA.makeBSarray s
     let sp = fromMaybe (error "S does not exist") $ BSA.elemIndex bsa 'S'
     let ep = fromMaybe (error "E does not exist") $ BSA.elemIndex bsa 'E'
     let mymap = M.fromList $ zip  (walk bsa sp sp ep) [0..]
     -- print( length mymap)
     let !ch =  map (cheats 20 mymap) (M.keys mymap)   `S.using` S.parListChunk 100 S.rdeepseq
     let mycheats = concat ch
     return (toInteger . length . filter (>=100) $ mycheats )