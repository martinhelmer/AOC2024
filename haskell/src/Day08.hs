{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day08 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
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
import AOCHelper (readInpByteSTring, Pos, Dir, tp, mapCollect, pairs)
import PosDir ((.+.), (.-.))
import qualified BSArray as BSA
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Bifunctor (bimap)
import Data.Containers.ListUtils (nubOrd)

example :: ByteString
example =
  [r|............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............|]

runex :: RunMe
runex =
  runMeByteString
    "Day 08 - example"
    (return example)
    part1
    (Just 14)
    part2
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "-- Day 8: Resonant Collinearity"
    (readInpByteSTring "day08.txt")
    part1
    (Just 400)
    part2
    (Just 1280)

---
data Resonance = ResOff | ResOn 

antinodes :: BSA.BSArray -> Resonance -> (Int, Int) -> (Int, Int) -> [ (Int, Int)]
antinodes bs res ix1 ix2 = case res of 
      ResOff -> filter (BSA.inRange bs) [ix2 .+. diff , ix1 .-. diff ]
      ResOn ->  (takeWhile (BSA.inRange bs) right) ++ (takeWhile (BSA.inRange bs) left)
    where diff = ix2 .-. ix1
          right = iterate (.+. diff) ix2
          left = iterate ((flip (.-.)) diff) ix1

collectAntennas :: BSA.BSArray -> M.Map Char [BSA.Index]
collectAntennas bs = 
    mapCollect (:[]) 
    . filter (\(c, _) -> c /= '.') 
    . map (\i -> (BSA.lookup bs i, i)) 
    . BSA.indices
    $ bs

go :: Monad m => ByteString -> Resonance -> m Integer
go s res = do 
      let bs = BSA.makeBSarray s
      return
        . toInteger 
        . length 
        . nubOrd 
        . concatMap (concatMap (uncurry (antinodes bs res)) . pairs) 
        . M.elems 
        . collectAntennas 
        $ bs 

part1 :: ByteString -> IO Integer
part1 s = go s ResOff 

part2 :: ByteString -> IO Integer
part2 s = go s ResOn 