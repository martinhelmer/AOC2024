{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day16 (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString)

import PosDir ( Pos(..), Dir(..), pdir,loc, (.->.), step, rl, rr, Loc )
import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import Algorithms (aStar, djikstra, paths, fromDist)
import qualified BSArray as BSA
import Data.Maybe (fromJust)
import BSArray (BSArray)
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
    (Just 7036)
    part2
    (Just 45)

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

steps :: BSArray -> Pos -> Int
steps grid p
          | BSA.lookup grid (loc p) == 'E' = 1
          | otherwise = move 0 p
    where  move currentsteps here
            | BSA.lookup grid (loc here)== 'E' = currentsteps
            | currentsteps == 0 = move (currentsteps + 1 ) (step here)
            | otherwise =
                case (dirsFromPos grid here) of
                    [] -> error "no dirsFromPos in steps"
                    [d] -> move (currentsteps + 1) (Pos (loc here .->. d) d)
                    _ ->  currentsteps

dirsFromPos :: BSArray -> Pos -> [Dir]
dirsFromPos grid pos =
        filter (\d -> BSA.lookup grid (loc pos .->. d) /= '#') [pdir pos, rl $ pdir pos, rr $ pdir pos]

nextnodes :: Bool -> BSArray -> Pos -> [(Pos, Score)]
nextnodes detailed grid p
          | BSA.lookup grid (loc p) == 'E' = []
          | BSA.lookup grid (loc p) == 'S' =  move 1000 (Pos (loc p) NORTH) ++ move 0 p
          | otherwise = move 0 p
    where  move currentscore here
            | BSA.lookup grid (loc here)== 'E' = [(Pos (loc here) NORTH, currentscore)]
            | currentscore == 0  = move (currentscore + 1 ) (step here)
            | otherwise =
                let maybeturncost d = if d /= (pdir here) then 1000 else 0 in
                case (dirsFromPos grid here, detailed) of
                    ([],_) ->[]
                    ([d],False) -> move (currentscore + 1 + maybeturncost d) (Pos (loc here .->. d) d)
                    (l,_) ->  map (\d -> (Pos (loc here) d,currentscore + maybeturncost d)) l

nn :: Bool -> BSArray -> Int -> [(Int, Score)]
nn d grid key = map (Data.Bifunctor.first (pos2key grid)) $ nextnodes d grid (key2pos grid key)

pos2key :: BSArray -> Pos -> Int
pos2key bs (Pos l d) = (BSA.rawIndex bs l )  * 4 + fromEnum d

key2pos :: BSArray -> Int -> Pos
key2pos bs key = let (ix, d) = key `divMod` 4  in Pos (BSA.rawIndex2Index bs ix) (toEnum d)

isEndnode :: BSArray -> Int -> Bool
isEndnode grid p = let el = fromJust . BSA.elemIndex grid $ 'E'
                       (Pos l _) = key2pos grid p in l == el

key2l :: BSArray -> Int -> PosDir.Loc
key2l grid k =let (Pos l _) = key2pos grid k in l

part1 :: ByteString -> IO Integer
part1 s = do
    let grid = BSA.makeBSarray s
    let sp = (Pos (fromJust . BSA.elemIndex grid $ 'S')) EAST
    let skey = pos2key grid sp
    let el = fromJust . BSA.elemIndex grid $ 'E'
    let gp key = key2l grid key == el
    return . toInteger . fromDist $ aStar [skey] gp (const 0) (nn False grid)

part2 :: ByteString -> IO Integer
part2 s = do
    let grid = BSA.makeBSarray s
    let sp = Pos (fromJust . BSA.elemIndex grid $ 'S') EAST
    let endkey = pos2key grid $ Pos (fromJust . BSA.elemIndex grid $ 'E') NORTH
    let skey = (pos2key grid sp)
    let (_,p)  = (djikstra skey (-1)  (nn False grid))
    let pths =  paths skey endkey p
    let ls = map (steps grid . key2pos grid) (nubOrd . concat $ pths)
    let crosses = map (key2l grid)   (nubOrd . concat $ pths)
    return . toInteger $ (sum ls - (length crosses - length (nubOrd crosses)))
