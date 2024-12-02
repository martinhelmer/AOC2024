{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Algorithms (aStar, Distance (..), distify) where

import Data.Bifunctor (second)
import qualified Data.IntPSQ as PSQ
import Data.IntMap.Strict as IM 
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Control.DeepSeq (deepseq, NFData (rnf))

-- import Debug.Trace (trace)

data Distance = Distance Int | Infinity deriving (Eq, Show)

(.+.) :: Distance -> Distance -> Distance
Infinity .+. _ = Infinity
_ .+. Infinity = Infinity
(Distance a) .+. (Distance b) = Distance (a + b)

fromDist :: Distance -> Int
fromDist (Distance i) = i
fromDist Infinity = undefined

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
type OpenSet = PSQ.IntPSQ Int ()


lookUp :: IntMap Int -> Key -> Distance
lookUp m i = maybe Infinity Distance (IM.lookup i m)

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