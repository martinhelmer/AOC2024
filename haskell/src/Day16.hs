{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day16 (runme, runex) where

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

import PosDir ( Pos(..), Dir(..), cardinalDirections, pdir,loc, opposite, (.->.), Loc, step )
import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, tp)
import Algorithms (aStar, djikstra, paths, fromDist)
import qualified BSArray as BSA
import Data.Maybe (fromJust)
import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable (foldl')
import BSArray (rows, cols, BSArray)
import Debug.Trace (trace)
import qualified Data.IntMap as IM
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Bifunctor

example :: ByteString
example =
  [r|###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
|]

runex :: RunMe
runex =
  runMeByteString
    "Day 16 - example"
    (return example)
    part1
    (Nothing)
    part2
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 16: Reindeer Maze ---"
    (readInpByteSTring "day16.txt")
    part1
    (Just 79404)
    part2
    (Just 451)

---
type Score = Int

movesFromCell :: BSArray -> Pos -> [Pos]
movesFromCell grid pos =  filter (\p -> BSA.lookup grid (loc p) /= '#') q
     where q = map (\dr -> Pos ((loc pos) .->. dr) dr) d
           d = cardinalDirections \\ [(opposite (pdir pos))]


dirsFromPos :: BSArray -> Pos -> [Dir]
dirsFromPos grid pos =
        filter (\d -> BSA.lookup grid (loc pos .->. d) /= '#')
        $ cardinalDirections \\ [(opposite (pdir pos))]

nextnodes :: Bool -> BSArray -> Pos -> [(Pos, Score)]
nextnodes detailed grid p = if BSA.lookup grid (loc p)== 'E' then [] else move 0 p
    where  move currentscore here
            | BSA.lookup grid (loc here)== 'E' = [(here, currentscore)]
            | currentscore == 0 = move (currentscore + 1 ) (step here)
            | otherwise =
                let maybeturncost d = if d /= (pdir here) then 1000 else 0 in
                case (dirsFromPos grid here, detailed) of
                    ([],_) ->[]
                    ([d],False) -> move (currentscore + 1 + maybeturncost d) (Pos (loc here .->. d) d)
                    (l,_) ->  map (\d -> (Pos (loc here) d,currentscore + maybeturncost d)) l

nn :: Bool -> BSArray -> Int -> [(Int, Score)]
nn d grid key = map (Data.Bifunctor.first (pos2key grid)) $ nextnodes d grid (key2pos grid key)

pos2key bs (Pos l d) = (BSA.rawIndex bs l )  * 4 + fromEnum d

key2pos bs key = let (ix, d) = key `divMod` 4  in Pos (BSA.rawIndex2Index bs ix) (toEnum d)

isEndnode grid el p = let (Pos l d) = key2pos grid p in l == el

key2l grid k =let (Pos l _) = key2pos grid k in l

part1 :: ByteString -> IO Integer
part1 s = do
    let grid = BSA.makeBSarray s
    let sp = Pos (fromJust . BSA.elemIndex grid $ 'S') EAST
    let skey = (pos2key grid sp)
    let el = fromJust . BSA.elemIndex grid $ 'E'
    let gp key = key2l grid key == el
    return . toInteger . fromDist $ aStar [skey] gp (const 0) (nn False grid)

part2 :: ByteString -> IO Integer
part2 s = do
    let grid = BSA.makeBSarray s
    let sp = Pos (fromJust . BSA.elemIndex grid $ 'S') EAST
    let skey = (pos2key grid sp)
    let el = fromJust . BSA.elemIndex grid $ 'E'
    let gp key = key2l grid key == el
    let (_,p)  = (djikstra (pos2key grid sp) (-1)  (nn True grid))
    -- print( filter (\w -> isEndnode grid el (fst w)) (IM.toList p) )
    -- print( map (\k -> IM.lookup k d) [1124,1125])
    let pths =  paths skey 1124 p
    let ls = map (key2l grid) (concat $ pths)
    return . toInteger $ (length . nubOrd $ ls )
