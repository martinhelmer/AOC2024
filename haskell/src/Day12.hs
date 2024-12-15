{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}

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
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Map as MAP
import Data.Maybe (catMaybes, mapMaybe, listToMaybe)
import PosDir ( (.+.), (.->.), cardinalDirections, rr, rl, Dir (..), opposite, loc2int, int2loc  )
import Data.List (foldl')
import qualified Data.List as L
import qualified Data.Bifunctor


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
type Canvas = Map P Color
type Members = Set P

downUpRightLeft :: [(Int, Int)]
downUpRightLeft = [(0,1), (0,-1), (1,0), (-1,0)]


paint :: Canvas -> Color -> P -> Members -> Members
paint can col  p m = foldr (paint can col) (S.insert (p) m) (neighboringNewMembers)
    where
        neighboringNewMembers = mapMaybe (mmm . (p .+.)) downUpRightLeft
        mmm loc
            |  S.member ( loc) m = Nothing
            |  (Just col') <- (M.lookup (loc) can)  = if col' == col then Just loc else Nothing
            | otherwise = Nothing


price :: Members -> Int
price m = area * perimeter
    where area = S.size m
          perimeter = sum . map (\p -> 4 - neighbors (p)) $ (S.elems m)
          neighbors p = length . filter (\d -> S.member ((p .+. d)) m) $ downUpRightLeft
---
delmany' :: Canvas -> Members -> Canvas
delmany' c m = foldl' (flip M.delete)  c (S.toList m)

delmany :: Canvas -> Members -> Canvas
delmany c = M.difference c . M.fromList . map (\t -> (t,undefined))  . S.toList


regions :: Canvas -> [Members]
regions can =
    case listToMaybe  . M.toList $ can of
        Nothing -> []
        Just (p, c) -> let region = paint can c (p) S.empty in region : regions (delmany' can region)

part1 :: ByteString -> IO Integer
part1 s = do
    let canvas =  BSA.toMap . BSA.makeBSarray $ s
    -- print (regions canvas)
    return . toInteger . sum . map price . regions $ canvas

-- m2im :: (Map BSA.Index Char) -> IntMap Char
-- m2im = M.fromList . map (Data.Bifunctor.first loc2int) . MAP.toList

price2 :: Members -> Int
price2 m = area * (sides m)
    where area = S.size m

part2 :: ByteString -> IO Integer
part2 s = do
    let canvas =  BSA.toMap . BSA.makeBSarray $ s
    -- return . toInteger . length $ r 
    return . toInteger . sum . map price2 . regions $ canvas

sides :: Members -> Int
sides m = (sum (map (countcorners m) (S.toList m))) `div` 2
countcorners :: Members -> P -> Int
countcorners m p =
        case sides' of
            [] -> 0
            [_,_,_,_] -> 8
            [side] -> checkcorner side (rr side) + checkcorner side (rl side)
            [NORTH, WEST] -> 2 + checkcorner NORTH EAST + checkcorner WEST SOUTH
            [side1, side2] -> if (side1 == (opposite side2)) then checkallcorners
                              else 2 + checkcorner side1 (rl side1) + checkcorner side2 (rr side2)
            l -> let unside = head (cardinalDirections L.\\ l) in 4 + checkcorner unside (rr unside) + checkcorner unside (rl unside)
    where sides' = filter (\d -> not . S.member ( (p .->. d)) $ m) cardinalDirections
          checkcorner s1 s2 = if S.member ( (p .->. s1 .->. s2)) m then 1 else 0
          checkallcorners = length . filter (\l -> S.member ( (p .+. l)) m)  $ [(-1,-1), (-1,1), (1,1), (1,-1)]
