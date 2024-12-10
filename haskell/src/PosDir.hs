{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PosDir ((.+.), (.-.), (.*->.), (.->.) , fromPos, toPos, rl, rr, Loc(..), Dir(..), Pos(..), mhdist, step, loc, pos2int, loc2int)
where

import Data.Bifunctor (bimap)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable (..))
import Data.List.Split.Internals (fromElem)

data Orientation = Vertical | Horizontal deriving (Show, Ord, Eq, Enum)

not' :: Orientation -> Orientation
not' Vertical = Horizontal
not' Horizontal = Vertical

directions :: Orientation -> [Dir]
directions o  = case o of
    Vertical -> [NORTH, SOUTH]
    Horizontal -> [EAST, WEST]

type Loc =  (Int, Int)
data Dir = NORTH | EAST | SOUTH | WEST deriving (Eq, Show, Enum, Ord)
data Pos = Pos !Loc !Dir deriving (Eq, Show, Ord)

loc :: Pos -> Loc
loc (Pos l _) = l

pos2int :: Pos -> Int
pos2int (Pos (x,y) d) = 4 * loc2int (x,y)  + fromEnum d 

loc2int :: Integral a => (a, a) -> a
loc2int (x,y) = let s = x + y in ((s * (s +1)) `div` 2 + y) 

instance Hashable Pos where
  hashWithSalt s p = s + hash (pos2int p) 

rl :: Dir -> Dir
rl NORTH = WEST
rl SOUTH = EAST
rl EAST = NORTH
rl WEST = SOUTH

rr :: Dir -> Dir
rr = rl . rl . rl

(.+.) :: Loc -> Loc -> Loc
(.+.) (a, b) (c, d) = (a + c, b + d)

(.-.) :: Loc -> Loc -> Loc
(.-.) (a, b) (c, d) = (a - c, b - d)


toPos :: (Int, Int) -> Loc
toPos (a, b) = (a, b)

fromPos :: Loc -> (Int, Int)
fromPos (a, b) = (a, b)

dToPos :: Dir -> Loc
dToPos NORTH = (-1, 0)
dToPos SOUTH = (1, 0)
dToPos EAST = (0, 1)
dToPos WEST = (0, -1)

(.->.) :: Loc -> Dir -> Loc
(.->.) p d = p .+. dToPos d

(.*->.) :: Int -> Dir -> Loc
(.*->.) i d = let  (a,b) = dToPos d in (i * a, i * b )

mhdist :: Loc -> Loc -> Int
mhdist (r, c) (r', c') = abs (r-r') + abs (c - c')

---

step :: Pos -> Pos
step (Pos l d) = Pos (l .->. d ) d