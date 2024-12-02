{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day17 (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import AOCHelper (readInpByteSTring)
import Algorithms (aStar, Distance (Distance), distify)
import BSArray (BSArray, makeBSarray)
import qualified BSArray as BSA
import Data.Char (digitToInt, ord)
import RunUtil (RunMe, runMeByteString)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Maybe (isJust, fromJust)
import Data.Bifunctor (second, Bifunctor (first))
import Control.DeepSeq (NFData, rnf, deepseq)

example :: ByteString
example =
  [r|2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533|]

runex :: RunMe
runex =
  runMeByteString
    "Day 17 - example"
    (return example)
    part1
    (Just 102)
    part2
    (Just 94)

runme :: RunMe
runme =
  runMeByteString
    "Day 17: Clumsy Crucible "
    (readInpByteSTring "day17.txt")
    part1
    (Just 674)
    part2
    (Just 773)

---
type Orientation = Int 

not' :: Orientation -> Orientation
not' o = 1-o 

directions :: Orientation -> [Dir]
directions o  = case o of
    1 -> [NORTH, SOUTH]
    0 -> [EAST, WEST]

directions' :: Orientation -> [(Int, Int)]
directions' o  = case o of
    1 -> [(-1,0), (1,0)]
    0 -> [(0,1), (0,-1)]

newtype Pos = Pos (Int, Int) deriving (Eq, Show, Ord)
data Dir = NORTH | EAST | SOUTH | WEST deriving (Eq, Show, Enum)

(.+.) :: Pos -> Pos -> Pos
(.+.) (Pos (a, b)) (Pos (c, d)) = Pos (a + c, b + d)

toPos :: (Int, Int) -> Pos
toPos (a, b) = Pos (a, b)

fromPos :: Pos -> (Int, Int)
fromPos (Pos (a, b)) = (a, b)

dToPos :: Dir -> Pos
dToPos NORTH = Pos (-1, 0)
dToPos SOUTH = Pos (1, 0)
dToPos EAST = Pos (0, 1)
dToPos WEST = Pos (0, -1)

(.->.) :: Pos -> Dir -> Pos
(.->.) p d = p .+. dToPos d

mhdist :: BSArray -> Pos' -> Pos' -> Int 
mhdist bsa p1 p2 = let (c,r) = BSA.rawIndex2Index bsa p1;
                        (c',r') = BSA.rawIndex2Index bsa p2
                      in  abs (r-r') + abs (c - c')

---

type State = Int 
   
neighbors1 :: BSArray -> State -> [(State, Int)]
neighbors1 bsa = neighbors bsa (take 3)

neighbors2 :: BSArray -> State -> [(State, Int)]
neighbors2 bsa s = let n = neighbors bsa (take 7 . drop 3) s in n 

neighbors ::BSArray -> ([(Pos', Int)] -> [(Pos', Int)]) -> State -> [(State, Int)]
neighbors bsa selector s = (\(p,d) -> (p*2+(1-o),d))
    <$> concatMap (selector . scan bsa (p) 0)
        (directions' (1 - o))
    where p = s `div` 2 
          o = s `rem` 2 

type Pos' = Int 

scan :: BSArray -> Pos' -> Int -> (Int,Int) -> [(Pos', Int)]
scan bsa p s (dr, dc) = case BSA.lookupMaybe bsa nextp of 
                        Nothing -> [] 
                        Just v ->  let !ss = s + ord v - ord '0' in  (BSA.rawIndex bsa nextp, ss):(scan bsa (BSA.rawIndex bsa nextp) ss (dr, dc))
        where nextp =  let (r,c) = BSA.rawIndex2Index bsa p  in (r+dr, c+dc)


find :: BSArray -> (BSArray -> Pos' -> [(Pos', Int)]) -> (Pos' -> Int) -> Distance
find bsa nf h = let sn = [(1), (0)] 
        in aStar sn hasArrived h (nf bsa )
        where hasArrived s = ((s `div` 2) == endpos bsa)


endpos :: BSArray -> Pos'
endpos bsa = BSA.rawIndex bsa (BSA.rows bsa -1, BSA.cols bsa -1  )


part1 :: ByteString -> IO Integer
part1 s = do
  let bsa = makeBSarray s 
  let h s = mhdist bsa (s `div` 2) (endpos bsa) 
  let (Distance v) = find bsa neighbors1 h
  return . toInteger $ v

part2 :: ByteString -> IO Integer
part2 s=  do   
  let bsa = makeBSarray s 
  let h s =  mhdist bsa (s `div` 2) (endpos bsa) 
  let (Distance r) = find bsa neighbors2 h
  return . toInteger $ r