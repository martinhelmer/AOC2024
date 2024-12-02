{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PosDir ((.+.), (.*->.), (.->.) , fromPos, toPos, rl, rr, Pos(..), Dir(..), mhdist)
where

import Data.Bifunctor (bimap)
import Control.DeepSeq (NFData)

data Orientation = Vertical | Horizontal deriving (Show, Ord, Eq, Enum)

not' :: Orientation -> Orientation
not' Vertical = Horizontal
not' Horizontal = Vertical

directions :: Orientation -> [Dir]
directions o  = case o of
    Vertical -> [NORTH, SOUTH]
    Horizontal -> [EAST, WEST]

newtype Pos = Pos (Int, Int) deriving (Eq, Show, Ord, NFData)
data Dir = NORTH | EAST | SOUTH | WEST deriving (Eq, Show, Enum, Ord)

rl :: Dir -> Dir
rl NORTH = WEST 
rl SOUTH = EAST 
rl EAST = NORTH 
rl WEST = SOUTH 

rr :: Dir -> Dir
rr = rl . rl . rl 

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

(.*->.) :: Int -> Dir -> Pos 
(.*->.) i d = let Pos (a,b) = dToPos d in Pos (i * a, i * b )

mhdist :: Pos -> Pos -> Int
mhdist (Pos (r, c)) (Pos (r', c')) = abs (r-r') + abs (c - c')

---