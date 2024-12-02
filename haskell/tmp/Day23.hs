{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BangPatterns #-}

module Day23 (runme, runex) where

import Text.RawString.QQ

import Control.DeepSeq
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
import Data.ByteString (ByteString, foldl1')
import qualified Data.ByteString.Char8 as BS


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import qualified BSArray as BSA
import BSArray (BSArray)
import PosDir ( Pos(..), fromPos, (.->.), Dir(..), rl, rr )
import Data.Map ( Map, (!) )
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet  as S
import Data.Maybe (fromMaybe, isJust, mapMaybe, fromJust, catMaybes)
import Debug.Trace (trace)
import Data.Foldable (foldl')
import Data.List (sort, sortOn)
import qualified Data.Ord
import Data.Bits (testBit, setBit)
import qualified Codec.Binary.UTF8.Generic as IN

example :: ByteString
example =
  [r|#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#|]

example2 :: ByteString
example2 =
     [r|#.###
#.###
#v###
#.>.#
#v#v#
#.>.#
###v#
###.#
###.#|]

data Environment = Environment {
                      bsa :: BSArray,
                      startpos :: Pos,
                      endpos :: Pos,
                      incl :: ( Environment -> Pos -> Dir -> Bool)
                      }
type IntGraph = IM.IntMap [(Int, Int)]
type Graph = Map Pos [(Pos, Int)]
type Edges = Map Pos (Pos, Int) -- neighboring point of node -> Node , distance
type Visited = Int
type Cache = IM.IntMap  Int

nextNode ::  Environment -> Int -> Pos -> Dir  -> (Pos, Pos, Int)
nextNode e@(Environment bsa' _ ep _) len p d
                     | p == ep = let xx = (p , p .->. NORTH, len ) in xx
                     | BSA.lookup bsa' (fromPos p) /= '.' && len > 1 = (fwd, p, len+1)
                     | BSA.lookup bsa' (fromPos fwd) /= '#' = nextNode e (len + 1) fwd d
                     | BSA.lookup bsa' (fromPos left) /= '#' = nextNode e (len + 1) left (rl d)
                     | BSA.lookup bsa' (fromPos right) /= '#' = nextNode e (len + 1) right (rr d)
                     | otherwise = error "oops"
    where fwd = p .->. d
          left = p .->. (rl d )
          right = p .->. (rr d )

doNode :: Environment -> Graph -> Edges -> Pos -> (Graph, Edges, [Pos])
doNode env g edges p = (M.insert p ((rnf nodes) `seq` nodes) g ,
                        M.union edges (M.fromList (mapMaybe snd nodesAndEdges)),
                        map (fst . fst) . filter (isJust . snd) $  nodesAndEdges)
  where nodes = let n = map fst nodesAndEdges in if (endpos env) `elem` (map fst n) then (filter (\(p, _) -> p == (endpos env)) n) else n
        nodesAndEdges = mapMaybe pedge [NORTH, EAST, SOUTH, WEST]
        pedge d' = if not includeme then Nothing else case (M.lookup (p .->. d') edges) of
              Just q -> Just (q, Nothing)
              Nothing -> let (n', e', i) = nextNode env 0 p d' in Just ( (n',i), Just (e', (p, i)))
              where includeme  = (incl env) env p d'

incl1 :: Environment -> Pos -> Dir -> Bool
incl1 e p d = case BSA.lookupMaybe (bsa e) (fromPos (p .->. d)) of
          Nothing -> False
          Just '>' -> d == EAST
          Just '<' -> d == WEST
          Just 'v' -> d == SOUTH
          Just '^' -> d == NORTH
          Just '.' -> True
          Just '#' -> False
          Just q  -> error ("missing:" <> show q)

incl2 :: Environment -> Pos -> Dir -> Bool
incl2 e p d =  case BSA.lookupMaybe (bsa e) (fromPos (p .->. d)) of
          Nothing -> False
          Just '>' -> not (d == WEST && count ">" == 2 && count "v" == 1)
          Just 'v' -> not (d == NORTH && count "v" == 2 && count ">" == 1)
          Just '#' -> False
          Just _  -> True
      where count s = (length . filter (maybe False (`elem` (s::String)) ) . map (\d' -> BSA.lookupMaybe (bsa e) (fromPos $ p .->. d')) $ [NORTH, EAST, SOUTH, WEST])

buildGraph :: Environment -> IntGraph
buildGraph env = IM.fromList $ map (\(k,v) -> (g2i ! k, map (\(k',d') -> (g2i ! k' , d')) v)) (M.toList g)
        where g = buildGraph' env M.empty M.empty [startpos env]
              g2i = M.fromAscList (zip (sort . M.keys $ g) [0..])

buildGraph' :: Environment -> Graph -> Edges -> [Pos] -> Graph
buildGraph' _ g _ [] = g
buildGraph' env g edges (p:ps) = buildGraph' env g' edges' (p' <> ps)
      where (g', edges', p') = if M.member p g then (g, edges, []) else  doNode env g edges p

member' key set = set `testBit` key

insert' key set = set `setBit` key

-- longestPath ::  IntGraph -> Int -> Visited -> Cache -> Int -> (Int, Cache)
-- longestPath  g endpos visited cache pos
--       | pos == endpos = (0, cache)
--       | otherwise = let key = (visited::Int) * 64 + pos in  case IM.lookup (key) cache of
--           Just d -> (d, cache)
--           Nothing ->
--               let (d'', c'' ) =  foldl' (\(md,c) (neighbor,distancetoneighbor) ->
--                      if member' neighbor visited then (md,c)
--                      else let (d',c') = longestPath g endpos (insert' pos visited) c neighbor  in (max md (distancetoneighbor +d'), c') ) (0,cache) ( (g IM.! pos ))
--               in (d'', IM.insert (visited * 64 +  pos) d'' c'' )
--               -- in (d'', c'' )

longestPath ::  IntGraph -> Int -> Visited -> Int -> Int
longestPath g endpos visited' pos'
                    | pos' == endpos = 0
                    | otherwise = foldl' (\md (neighbor,distancetoneighbor) ->
                                    if member' neighbor visited' then md
                                    else let d' = longestPath g endpos ( insert' pos' visited') neighbor  in max md (distancetoneighbor + d'))
                                  0
                                  ( (g IM.! pos' ))


-- longestPath ::  IntGraph -> Int -> Visited -> Int -> Int
-- longestPath  g endpos visited pos
--       | pos == endpos = 0
--       | otherwise = let !l = map (\(neighbor, distancetoneighbor) -> let !d' = longestPath g endpos ( insert' pos visited) neighbor  in (distancetoneighbor + d'))  (filter (\(n,_) -> not . member' n $ visited) (g IM.! pos ))
--                     in case l of 
--                       [] -> 0 
--                       ll -> maximum ll 

srt l = if sum ( map (\(Pos (a,b), c) -> a+b+c) l ) < 0 then error "" else  l

runex :: RunMe
runex =
  runMeByteString
    "Day 23 - example"
    (return example)
    part1
    (Just 94)
    part2
    (Just 154)

runme :: RunMe
runme =
  runMeByteString
    "Day 23: A Long Walk."
    (readInpByteSTring "day23.txt")
    part1
    (Just 2178)
    part2
    (Just 6486)
---

part1 :: ByteString -> IO Integer
part1 s = do
  let env = let b = BSA.makeBSarray s in Environment b (Pos (0,1)) (Pos (BSA.rows b -1, BSA.cols b -2)) incl1
  let graph = buildGraph env
  return . toInteger  $ (longestPath graph  (fst . IM.findMax $ graph) 0 0)

part2 :: ByteString -> IO Integer
part2 s =  do
  let env = let b = BSA.makeBSarray s in Environment b (Pos (0,1)) (Pos (BSA.rows b -1, BSA.cols b -2)) incl2
  let graph = buildGraph env
  return . toInteger  $ (longestPath graph (fst . IM.findMax $ graph) 0 0)
