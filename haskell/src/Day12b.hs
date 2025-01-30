{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Day12b (runme, runex) where

import AOCHelper (readInpByteSTring)
import qualified BSArray as BSA

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Array (inRange)
import Data.Array.ST ( STUArray )
import qualified Data.Array.MArray as MA
import qualified Data.Array.Unboxed as A
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as S
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
import RunUtil (RunMe, runMeByteString)
import Text.RawString.QQ
import Data.Array.Base ((!?))
import Data.Char (ord)

runex :: RunMe
runex =
  runMeByteString
    "Day 12 - example"
    (return example1)
    part1
    (Just 1930)
    part2
    (Just 1206)

-- rewriting to Marr saves about 20 ms (~25%)
runme :: RunMe
runme =
  runMeByteString
    "-- Day 12b: Garden Groups (Marr)"
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

type P = (Int, Int)

type Color = Char

type Canvas = Map P Color

type Members = Set P

type STUGrid s = (STUArray s PosDir.Loc Int)

type Grid = A.UArray PosDir.Loc Int

downUpRightLeft :: [(Int, Int)]
downUpRightLeft = [(0, 1), (0, -1), (1, 0), (-1, 0)]

paint ::
  Canvas ->
  STUGrid s ->
  Int -> -- paint area code
  Char -> -- paint area color
  PosDir.Loc -> -- paint here
  ST s ()
paint canvas grid code color loc =
   do
        bounds <- MA.getBounds grid
        let locsTOCheck = filter (inRange bounds) $ map (loc PosDir..+.)  downUpRightLeft
        MA.writeArray grid loc code
        mapM_
          ( \l ->
              MA.readArray grid l >>= \r' ->
                when ((r' == ord color)) -- && (canvas M.! l == color))
                $ paint canvas grid code color l
          )
          locsTOCheck

totalPaint :: Canvas -> ST s (Grid)
totalPaint canvas =
  let keys = M.keys canvas
   in do
        grid <- MA.newListArray (minimum keys, maximum keys) ((map ord $ M.elems canvas)::[Int])
        mapM_
          ( \k ->
              MA.readArray grid k
                >>= (flip when)
                  (paint canvas grid (200 + PosDir.loc2int k) (canvas M.! k) k)
                  . (< 200)
          )
          keys
        MA.freeze grid


price :: Members -> Int
price m = area * perimeter
  where
    area = S.size m
    perimeter = sum . map (\p -> 4 - neighbors (p)) $ (S.elems m)
    neighbors p = length . filter (\d -> S.member ((p PosDir..+. d)) m) $ downUpRightLeft

---
regions :: Map P Color -> (Grid, Map Int (Set Loc))
regions can =
  let finishedGrid = (runST (totalPaint can)) :: Grid
      q =   map (\(a,b) -> (b,S.singleton a)) $ A.assocs finishedGrid
   in (finishedGrid, M.fromListWith (S.union) q )

part1 :: ByteString -> IO Integer
part1 s = do
  let canvas = BSA.toMap . BSA.makeBSarray $ s
      (grid, memb) = regions canvas
  return . toInteger $ M.foldlWithKey (price1 grid) 0 memb

price1 :: Grid -> Int -> Int  -> Members -> Int 
price1 grid  acc setkey set = S.size set * perimeter + acc
      where perimeter =  sum . map (\p -> 4 - neighbors p) $ (S.elems set)
            neighbors p = length . filter (\d -> ismemb (p PosDir..+. d)) $ downUpRightLeft
            ismemb p =  grid !? p  == Just setkey 

price2 :: Members -> Int
price2 m = area * (sides m)
  where
    area = S.size m

part2 :: ByteString -> IO Integer
part2 s = do
  let canvas = BSA.toHashMap . BSA.makeBSarray $ s
  -- return . toInteger . length $ r
  return  0 --toInteger . sum . map price2 . regions $ canvas

sides :: Members -> Int
sides m = (sum (map (countcorners m) (S.toList m))) `div` 2

countcorners :: Members -> P -> Int
countcorners m p =
  case sides' of
    [] -> 0
    [_, _, _, _] -> 8
    [side] -> checkcorner side (PosDir.rr side) + checkcorner side (PosDir.rl side)
    [PosDir.NORTH, PosDir.WEST] -> 2 + checkcorner PosDir.NORTH PosDir.EAST + checkcorner PosDir.WEST PosDir.SOUTH
    [side1, side2] ->
      if (side1 == (PosDir.opposite side2))
        then checkallcorners
        else 2 + checkcorner side1 (PosDir.rl side1) + checkcorner side2 (PosDir.rr side2)
    l -> let unside = head (PosDir.cardinalDirections L.\\ l) in 4 + checkcorner unside (PosDir.rr unside) + checkcorner unside (PosDir.rl unside)
  where
    sides' = filter (\d -> not . S.member ((p PosDir..->. d)) $ m) PosDir.cardinalDirections
    checkcorner s1 s2 = if S.member ((p PosDir..->. s1 PosDir..->. s2)) m then 1 else 0
    checkallcorners = length . filter (\l -> S.member ((p PosDir..+. l)) m) $ [(-1, -1), (-1, 1), (1, 1), (1, -1)]
