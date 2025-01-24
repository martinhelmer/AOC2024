{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE PackageImports #-}

module Day12 (runme, runex) where

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
import AOCHelper (readInpByteSTring, Pos, Dir, tp)
import qualified BSArray as BSA

import Data.Map (Map)
-- import Data.Map (IntMap)
import Data.Set (Set, (\\))
import qualified Data.Set as SS
import qualified Data.IntSet as S
import qualified Data.HashMap.Strict as M
import Data.Maybe (catMaybes, mapMaybe, listToMaybe)
import PosDir ( (.+.), (.->.), cardinalDirections, rr, rl, Dir (..), opposite, loc2int, int2loc, Loc  )
import Data.List (foldl')
import qualified Data.List as L
import qualified Data.Bifunctor
import Data.HashMap.Strict (HashMap)
import BSArray (BSArray)
import qualified BSArray as BSA


runex :: RunMe
runex =
  runMeByteString
    "Day 12 - example"
    (return example1)
    part1
    (Just 1930)
    part2
    (Just 1206)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 12: Garden Groups ---"
    (readInpByteSTring "day12.txt")
    part1
    (Just 1477924)
    part2
    (Just 841934)

---
example0 :: ByteString
example0 =
  [r|AAAA
BBCD
BBCC
EEEC|]

example1 :: ByteString
example1 =
  [r|RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE|]

example2 :: ByteString
example2 =
  [r|AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA|]

type P = (Int,Int)
type Color = Char
type Canvas = HashMap P Color
type Members = S.IntSet

downUpRightLeft :: [(Int, Int)]
downUpRightLeft = [(0,1), (0,-1), (1,0), (-1,0)]


paint :: BSArray -> S.IntSet -> Color -> P -> Members -> Members
paint bsa can col  p m = foldl' (flip (paint bsa can col)) (S.insert (ix2i p) m) (neighboringNewMembers)
    where
        ix2i = BSA.index2Int bsa
        neighboringNewMembers :: [P]
        neighboringNewMembers = mapMaybe (mmm . (p .+.)) downUpRightLeft
        mmm :: P -> Maybe P
        mmm loc
            |  S.member (ix2i loc) m = Nothing
            |  (Just col') <- (BSA.lookupMaybe bsa (loc))  = if col' == col then Just loc else Nothing
            | otherwise = Nothing

price :: BSArray -> Members -> Int
price bsa m = area * perimeter
    where area = S.size m
          perimeter = sum (map ((\p -> 4 - neighbors (p)) . BSA.int2Index bsa) (S.toList m))
          neighbors p = length . filter (\d -> S.member (BSA.index2Int bsa (p .+. d)) m) $ downUpRightLeft
---

regions :: BSArray -> S.IntSet -> [Members]
regions bsa can =
    case listToMaybe  . S.toList $ can of
        Nothing -> []
        Just p' -> let p = (BSA.int2Index bsa p')
                       region = paint bsa can (BSA.lookup bsa p) (p) S.empty in region : regions bsa  (S.difference can region)

part1 :: ByteString -> IO Integer
part1 s = do
    let bsa = BSA.makeBSarray $ s
        bsapoints = S.fromList $ map (BSA.index2Int bsa) (BSA.indices bsa)
    return . toInteger . sum . map (price bsa) $ regions bsa bsapoints

-- m2im :: (Map BSA.Index Char) -> IntMap Char
-- m2im = M.fromList . map (Data.Bifunctor.first loc2int) . MAP.toList

price2 :: BSArray -> Members -> Int
price2 bsa m = area * (sides (SS.fromList (map (BSA.int2Index bsa) $ S.toList m)))
             where area = S.size m

part2 :: ByteString -> IO Integer
part2 s = do
    let bsa = BSA.makeBSarray $ s
        bsapoints = S.fromList $ map (BSA.index2Int bsa) (BSA.indices bsa)
    return . toInteger . sum . map (price2 bsa) $ regions bsa bsapoints

sides :: SS.Set P -> Int
sides m = (sum (map (countcorners m) (SS.toList m))) `div` 2

countcorners :: SS.Set P -> P -> Int
countcorners m p =
        case sides' of
            [] -> 0
            [_,_,_,_] -> 8
            [side] -> checkcorner side (rr side) + checkcorner side (rl side)
            [NORTH, WEST] -> 2 + checkcorner NORTH EAST + checkcorner WEST SOUTH
            [side1, side2] -> if (side1 == (opposite side2)) then checkallcorners
                              else 2 + checkcorner side1 (rl side1) + checkcorner side2 (rr side2)
            l -> let unside = head (cardinalDirections L.\\ l) in 4 + checkcorner unside (rr unside) + checkcorner unside (rl unside)
    where sides' = filter (\d -> not . SS.member ( (p .->. d)) $ m) cardinalDirections
          checkcorner s1 s2 = if SS.member ( (p .->. s1 .->. s2)) m then 1 else 0
          checkallcorners = length . filter (\l -> SS.member ( (p .+. l)) m)  $ [(-1,-1), (-1,1), (1,1), (1,-1)]
