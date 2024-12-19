{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE TupleSections #-}

module Day18 (runme, runex) where

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
  char,
 )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.IntMap as IM

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, Parseable)
import qualified BSArray as BSA
import PosDir (cardinalDirections, Loc, (.->.))
import Algorithms (djikstra, aStar, fromDist, Distance (Infinity))
import qualified Data.Bifunctor
import Data.Maybe (fromJust, fromMaybe)

example :: ByteString
example =
  [r|5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
|]

runex :: RunMe
runex =
  runMeByteString
    "Day 18 - example"
    (return example)
    part1
    (Nothing)
    part2
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 18: RAM Run ---"
    (readInpByteSTring "day18.txt")
    part1
    (Just 294)
    part2
    (Just 3122)

---
data Memory = Memory { size :: Int
                     , incoming :: M.Map Loc Int} deriving (Show)

parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

parseLoc :: Parser (Int,Int)
parseLoc = (,) <$> decimal <* char ',' <*> decimal

nextnodes :: Memory -> Int -> Loc -> [(Loc, Int)]
nextnodes (Memory siz inc) limit loc = (,1) <$> filter ok (map (loc .->.) cardinalDirections)
    where ok l@(x,y) = x >= 0 && y >= 0 && x < siz && y < siz
                       && (fromMaybe 999999 (M.lookup l inc) >= limit)

nn :: Memory -> Int -> Int -> [(Int, Int)]
nn m n k = map (Data.Bifunctor.first (loc2key m)) $ nextnodes m n (key2loc m k)

loc2key :: Memory -> Loc -> Int
loc2key m (x,y) = y + (size m) * x

key2loc :: Memory -> Int -> Loc
key2loc m k = k `divMod` (size m)

endkey :: Memory -> Int
endkey m =let s = size m - 1 in (loc2key m (s,s)) 

startkey :: Memory -> Int
startkey m = loc2key m (0, 0)

binomsearch :: Integral t => (t -> Distance) -> t -> t -> t
binomsearch f lower upper
      | upper - lower == 1 = lower
      | upper <= lower = error "binom error"
      | otherwise =  let mid = lower + ((upper - lower) `div` 2) in
          case f mid of
            Infinity -> binomsearch f lower mid
            _ -> binomsearch f mid upper

shortest :: Memory -> Int -> Distance
shortest m n = aStar [startkey m] (endkey m ==) (const 0) (nn m n)

part1 :: ByteString -> IO Integer
part1  s=  do
    let siz = 71
    let input =  zip (map (parse' parseLoc) (BS.lines s)) ([0..]::[Int])
    let m = Memory siz (M.fromList input)
    return $ toInteger . fromDist $ shortest m 1024

part2 :: ByteString -> IO Integer
part2 s = do
    let siz = 71
    let input =  zip (map (parse' parseLoc) (BS.lines s)) ([0..]::[Int])
    let m = Memory siz (M.fromList input)
    let q = binomsearch (shortest m) 1024 (length input)
    -- outputs (ab,cd) as abcd (framework needs result to be integer)
    return $ toInteger $ let (x,y) = fst (input !! q) in x * 100 + y 