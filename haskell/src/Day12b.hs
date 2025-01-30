{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day12b (runme, runex) where

import AOCHelper (readInp)
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Array (inRange)
import Data.Array.Base (unsafeFreeze, (!?))
import qualified Data.Array.MArray as MA
import Data.Array.ST (STUArray)
import qualified Data.Array.Unboxed as A
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import PosDir
  ( Dir (..),
    Loc,
    cardinalDirections,
    loc2int,
    opposite,
    rl,
    rr,
    (.+.),
    (.->.),
  )
import RunUtil (RunMe, runMeString)
import Text.RawString.QQ
import "unordered-containers" Data.HashSet (HashSet)
import qualified "unordered-containers" Data.HashSet as HS

runex :: RunMe
runex =
  runMeString
    "Day 12 - example"
    (return example1)
    part1
    (Just 1930)
    part2
    (Just 1206)

-- rewriting to Marr saves about 20 ms (~25%) -> 65 ms
-- only use a grid / marr                     -> 41 ms 
runme :: RunMe
runme =
  runMeString
    "-- Day 12b: Garden Groups (Marr)"
    (readInp "day12.txt")
    part1
    (Just 1477924)
    part2
    (Just 841934)

---
example0 :: String
example0 =
  [r|AAAA
BBCD
BBCC
EEEC|]

example1 :: String
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

parseIntoArray :: [Char] -> A.UArray (Int, Int) Int
parseIntoArray s = A.listArray bounds (map ord $ concat l)
  where
    bounds = ((0, 0), (length l - 1, length (head l) - 1))
    l = lines s

downUpRightLeft :: [(Int, Int)]
downUpRightLeft = [(0, 1), (0, -1), (1, 0), (-1, 0)]

price2 :: Grid -> Members -> Int
price2 g m = area * sides
  where
    area = HS.size m
    sides = (sum (map (countcorners g) (HS.toList m))) `div` 2

countcorners :: Grid -> P -> Int
countcorners g p =
  case sides' of
    [] -> 0
    [_, _, _, _] -> 8
    [side] -> checkcorner side (PosDir.rr side) + checkcorner side (PosDir.rl side)
    [NORTH, WEST] -> 2 + checkcorner NORTH EAST + checkcorner WEST SOUTH
    [side1, side2] ->
      if (side1 == (PosDir.opposite side2))
        then checkallcorners
        else 2 + checkcorner side1 (PosDir.rl side1) + checkcorner side2 (PosDir.rr side2)
    l -> let unside = head (cardinalDirections L.\\ l)
          in 4 + checkcorner unside (PosDir.rr unside) + checkcorner unside (PosDir.rl unside)
  where
    pSetCode = Just (g A.! p)
    isMember p' = pSetCode == g A.!? p' 
    sides' = filter (\d -> not . isMember $ (p PosDir..->. d)) cardinalDirections
    checkcorner s1 s2 = if isMember (p PosDir..->. s1 PosDir..->. s2) then 1 else 0
    checkallcorners = length 
                      . filter (\l -> isMember (p PosDir..+. l)) 
                      $ [(-1, -1), (-1, 1), (1, 1), (1, -1)]

---
type P = (Int, Int)

type Color = Int

type Members = HashSet P

type STUGrid s = (STUArray s PosDir.Loc Int)

type Grid = A.UArray PosDir.Loc Int

paint ::
  STUGrid s ->
  Int -> -- paint area code
  Int -> -- paint area color
  PosDir.Loc -> -- paint here
  ST s ()
paint grid code color loc =
  do
    bounds <- MA.getBounds grid
    let locsTOCheck = filter (inRange bounds) $ map (loc PosDir..+.) downUpRightLeft
    MA.writeArray grid loc code
    mapM_
      ( \l ->
          MA.readArray grid l >>= \r' ->
            when ((r' == color)) $ -- && (canvas M.! l == color))
              paint grid code color l
      )
      locsTOCheck

totalPaint :: Grid -> ST s (Grid)
totalPaint canvas =
  do
    grid <- MA.thaw canvas
    mapM_
      ( \ix ->
          MA.readArray grid ix
            >>= (flip when)
              (paint grid (200 + PosDir.loc2int ix) (canvas A.! ix) ix)
              . (< 200)
      )
      (A.indices canvas)
    unsafeFreeze grid

regions :: Grid -> (Grid, IntMap (Members))
regions can =
  let finishedGrid = (runST (totalPaint can)) :: Grid
      q = map (\(a, b) -> (b, HS.singleton a)) $ A.assocs finishedGrid
   in (finishedGrid, IM.fromListWith (HS.union) q)

price1 :: Grid -> Int -> Int -> Members -> Int
price1 grid acc setkey set = area * perimeter + acc
  where
    area = HS.size set 
    perimeter = HS.foldl' (\acc' p -> acc' + 4 - neighbors p) 0  set 
    neighbors p = length . filter (\d -> ismemb (p PosDir..+. d)) $ downUpRightLeft
    ismemb p = grid !? p == Just setkey

part1 :: String -> IO Integer
part1 s = do
  let canvas = parseIntoArray (s)
      (grid, memb) = regions canvas
  return . toInteger $ IM.foldlWithKey (price1 grid) 0 memb

part2 :: String -> IO Integer
part2 s = do
  let canvas = parseIntoArray (s)
      (grid, memb) = regions canvas
  return . toInteger . sum . map (price2 grid) $ IM.elems memb