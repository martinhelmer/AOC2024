{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Day22 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  decimal,
  char, 
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
import qualified Data.IntPSQ as PSQ
import qualified Data.Map as M
import qualified Data.IntMap as IM
import  Data.IntMap (IntMap, (!))
import AOCHelper (Dir, Pos, readInpByteSTring, tp)
import qualified BSArray as BSA
import RunUtil (RunMe, runMeByteString)
import Data.Maybe (fromMaybe)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.List (sortOn, nub, delete)
import Data.Foldable (foldl')
import Data.Containers.ListUtils (nubOrd, nubInt)

example :: ByteString
example =
  [r|1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9|]

runex :: RunMe
runex =
  runMeByteString
    "Day 22 - example"
    (return example)
    part1
    (Just 5)
    part2
    (Just 7)

runme :: RunMe
runme =
  runMeByteString
    "Day 22: Sand Slabs"
    (readInpByteSTring "day22.txt")
    part1
    (Just 509)
    part2
    (Just 102770)

comma :: Parser Char
comma = AP.char ','

parseSlab :: Parser PreSlab
parseSlab =
  (\a b c d e f -> ((a,b,c),(d,e,f)))
    <$> decimal
    <* comma
    <*> decimal
    <* comma
    <*> decimal
    <* char '~'
    <*> decimal
    <* comma
    <*> decimal
    <* comma
    <*> decimal

parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s
---

type SlabID = Int

type Bottom = M.Map (Int, Int) (Int,SlabID)
type SlabMap = IntMap Slab
---
data Slab = Slab
  { slabid :: SlabID
  , altitude :: Int
  , restsUpon :: [Int]
  , isUnder :: [Int]
  } deriving (Eq, Ord, Show)

type PreSlab = ((Int,Int,Int), (Int,Int,Int))

x' :: (a, b, c) -> a
x' (x,_,_) = x
y' :: (a, b, c) -> b
y' (_, y,_) = y
z' :: (a, b, c) -> c
z' (_, _, z) = z

fallSlab :: (Bottom, SlabMap) -> PreSlab -> (Bottom, SlabMap)
fallSlab (bottom, slabmap) _preslab@(p1, p2) = (newbottom topofnewslab newslabid, newslabmap)
 where
  xy' = [(i, j) | i <- [x' p1 .. x' p2], j <- [y' p1 .. y' p2]]
  topofnewslab = maxbottom' + (z' p2 - z' p1 + 1)
  newslabid = maybe 1 ((+) 1 . fst) (IM.lookupMax slabmap)
  newslabmap = foldl' (flip (IM.adjust (\e -> e{isUnder = newslabid : isUnder e}))) (IM.insert newslabid newslab slabmap) restsUpon'
  (maxbottom', restsUpon') = maxbottom bottom xy'
  newslab = Slab newslabid topofnewslab restsUpon' []

  newbottom ::  Int -> SlabID-> Bottom
  newbottom  newlevel slabid' = M.union (M.fromList $ map (\p -> (p, (newlevel, slabid'))) xy') bottom

  maxbottom :: Bottom -> [(Int, Int)] -> (Int, [SlabID])
  maxbottom bottom =
    foldl
      ( \acc@(alt, ids) p ->
          let (alt', id') = fromMaybe (-1, 0) (M.lookup p bottom)
          in if alt' > alt then (alt', [id']) else if alt' == alt then (alt, nubInt (id' : ids)) else acc
      )
      (0, [])

calcFallers :: SlabMap -> Slab -> Int 
calcFallers sm s = (calcFallers' sm (PSQ.singleton 0 0 s{ restsUpon = []} )) - 1 

calcFallers' :: SlabMap -> PSQ.IntPSQ Int Slab -> Int
calcFallers' m q = case PSQ.minView q of
  Nothing -> 0
  Just (_, _, slab, qtail) ->
    if not (null (restsUpon slab))
      then calcFallers' m qtail
      else 1 + (calcFallers' m (foldl f qtail (isUnder slab)))
   where
    f q' sid = PSQ.insert (slabid slab') ( altitude slab') slab' q'
     where
      slab' = let s'' = maybe (m ! sid) (snd) (PSQ.lookup sid q) in s''{restsUpon = delete (slabid slab) (restsUpon s'')}


canberemoved :: SlabMap -> Slab -> Bool
canberemoved m s = all (wontfall . (m !)) (isUnder s )
  where wontfall s' = length (restsUpon s') > 1

part1 :: ByteString -> IO Integer
part1 s = do
  let preslabs = sortOn (\((_,_,z),_) -> z ) . map (parse' parseSlab) $ (BS.lines s)
  let (_, sm) = foldl' fallSlab (M.empty, IM.empty) preslabs

  return . toInteger . IM.size . IM.filter (canberemoved sm) $ sm 

part2 :: ByteString -> IO Integer
part2 s = do 
  let preslabs = sortOn (\((_,_,z),_) -> z ) . map (parse' parseSlab) $ (BS.lines s)
  let (_, sm) = foldl' fallSlab (M.empty, IM.empty) preslabs
  return . toInteger . sum . map (calcFallers sm) . IM.elems $ sm 
