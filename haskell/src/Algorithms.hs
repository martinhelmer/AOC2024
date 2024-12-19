{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Algorithms (aStar, Distance (..), distify, djikstra, paths, fromDist) where

import Data.Bifunctor (second)
import qualified Data.IntPSQ as PSQ
import Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Maybe (fromMaybe, fromJust)
import Control.DeepSeq (deepseq, NFData (rnf))

import Debug.Trace (trace)
import Data.Graph.Inductive (neighbors, gmap)

data Distance = Distance Int | Infinity deriving (Eq, Show)

(.+.) :: Distance -> Distance -> Distance
Infinity .+. _ = Infinity
_ .+. Infinity = Infinity
(Distance a) .+. (Distance b) = Distance (a + b)

fromDist :: Distance -> Int
fromDist (Distance i) = i
fromDist Infinity = error "Trying to extract infinity"

instance Ord Distance where
    (<=) :: Distance -> Distance -> Bool
    Infinity <= Infinity = True
    Infinity <= _ = False
    _ <= Infinity = True
    Distance a <= Distance b = a <= b

instance NFData Distance where
  rnf Infinity = ()
  rnf (Distance q) = q `seq` ()

type GMap = IM.IntMap Int
type PrevMap = IM.IntMap [Int]
type OpenSet = PSQ.IntPSQ Int ()

type DQ = PSQ.IntPSQ Node Int
type Node = Int
type Dist = Int

lookUp :: IntMap Int -> Key -> Distance
lookUp m i = maybe Infinity Distance (IM.lookup i m)


istrace = False

trace'  s= if istrace then trace s else id

-- | djikstra with history
--
-- > sourcenode targetnode nn
--
djikstra :: Key -> Key -> (Node -> [(Node, Dist)]) -> (GMap, PrevMap)
djikstra source target nf = djikstra' target (PSQ.singleton source 0 0) (IM.singleton source 0) IM.empty nf


djikstra' :: Key -> DQ -> GMap ->  PrevMap -> (Node -> [(Node, Dist)]) -> (GMap, PrevMap )
djikstra' target q  dist  prev nf
    | PSQ.null q =  (dist, prev)
    | otherwise =
        let (u, _, v , poppedQ) = (trace' $ "q:" ++ show q) fromJust $ PSQ.minView q
            currentDist = lookUp dist u;
            neighbors = (trace' $ show (nf u)) nf u
            (dist'', prev'', q'') = Data.List.foldl' go (dist, prev, poppedQ) neighbors
              where
                go :: (GMap, PrevMap, DQ) ->(Key, Dist) -> (GMap, PrevMap, DQ)
                go (dist', prev', q') (nkey, ndist) =
                      case compare (Distance ndist .+. currentDist) (lookUp dist' nkey) of
                        GT ->  trace' ("huh: " ++ show (dist', prev', q', nkey, ndist, currentDist)) (dist', prev', q')
                        EQ ->  (dist', IM.insertWith (<>) nkey [u] prev', q')
                        LT ->  ( IM.insert nkey (ndist + fromDist currentDist) dist'
                                    , IM.insert nkey [u] prev'
                                    , PSQ.insert nkey ndist (v+1) q'
                                    )
        in
            if False && u == target then (dist, prev) else (trace' $ show q'') djikstra' target q'' dist'' prev'' nf


paths :: Key -> Key -> PrevMap -> [[Key]]
paths source target prev | source == target = [[target]]
                         | otherwise = let prevs = fromMaybe (error "AA") $ IM.lookup target prev
                                           allprevs = concatMap (\p -> paths source p prev) prevs
                                       in  Prelude.map (target :) allprevs

-- | 'aStar': does stuff stuff: 
--
-- > startnode goalpred h neighbors
--
-- /Since: 1.1.0.0/
aStar :: [Int] -> (Int -> Bool) -> (Int -> Int) -> (Int -> [(Int, Int)]) -> Distance
aStar startnodes goalpred h nf = aStar' openSet gScores goalpred h nf
  where
    openSet = PSQ.fromList $ Prelude.map (\sn -> (sn, h sn, ())) startnodes
    gScores = IM.fromList $ Prelude.map (\sn -> (sn, 0)) startnodes



aStar' :: OpenSet -> GMap -> (Int -> Bool) -> (Key -> Int) -> (Int -> [(Int, Int)]) -> Distance
aStar' openSet gScores goalpred h neighbors
    | PSQ.null openSet = Infinity
    | goalpred current = Distance currentG
    | otherwise = aStar' updatedOpenSet gScores'' goalpred h neighbors
  where
    (current, _, _, poppedOpenSet) = fromMaybe undefined (PSQ.minView openSet)
    currentG = gScores IM.! current

    (updatedOpenSet, gScores'') =  let n = neighbors current in Data.List.foldl' go (poppedOpenSet, gScores) n

    go (os, gs) (nn, d)
        | Distance tentdist >= lookUp gs nn = (os, gs)
        | otherwise =
            ( PSQ.insert nn (tentdist  + h nn) () os
            , IM.insert nn tentdist gs
            )
      where
        tentdist = currentG +  d

--
distify :: [(a, Int)] -> [(a, Distance)]
distify l = second Distance <$> l

-- neighborsFromMapWithDist :: (Hashable a, Ord a) => Map a [(a, Int)] -> a -> [(a, Distance)]
-- neighborsFromMapWithDist m a = distify (m M.! a)

-- neighborsFromMap :: (Hashable a, Ord a) => Map a [a] -> a -> [(a, Distance)]
-- neighborsFromMap m a = (,Distance 1) <$> (m M.! a)