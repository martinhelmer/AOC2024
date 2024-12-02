{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Day16 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import Control.DeepSeq
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import Data.Map (Map)
import Data.IntSet (IntSet)


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import BSArray
import qualified BSArray as BSA
import Prelude hiding (traverse)

example :: ByteString
example =
  [r|.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....|]

runex :: RunMe
runex =
  runMeByteString
    "Day 16 - example"
    (return example)
    part1
    (Just 46)
    part2
    (Just 51)

runme :: RunMe
runme =
  runMeByteString
    "Day 16: The Floor Will Be Lava"
    (readInpByteSTring "day16.txt")
    part1
    (Just 7482)
    part2
    (Just 7896)

---
newtype Pos = Pos (Int, Int) deriving (Eq, Show)
data Dir = NORTH | EAST | SOUTH | WEST deriving (Eq, Show)

(.+.) :: Pos -> Pos -> Pos
(.+.) (Pos (a,b)) (Pos (c,d)) = Pos (a+c, b+d)

toPos :: (Int, Int) -> Pos
toPos (a,b) = Pos (a,b)

fromPos :: Pos -> (Int, Int)
fromPos (Pos (a,b)) = (a, b)

dToPos :: Dir -> Pos
dToPos NORTH = Pos (-1,0)
dToPos SOUTH = Pos (1, 0)
dToPos EAST = Pos (0,1)
dToPos WEST = Pos (0,-1)

(.->.) :: Pos -> Dir -> Pos
(.->.) p d = p .+. dToPos d
--- 

data Path = Path {pathPoints :: IntSet
                ,pathEndPoint :: Maybe Int
                } deriving (Show)

getPath :: BSArray -> Pos -> Dir -> Path
getPath bsa p d = let (l, ep) = getPath' [fromPos p] p d in Path (IS.fromList (map (BSA.intIndex bsa) l)) (BSA.intIndex bsa <$> ep)
  where getPath' :: [(Int,Int)] -> Pos -> Dir -> ([(Int,Int)], Maybe (Int,Int))
        getPath' path pos' dir' =
          case (nextcell, dir')  of
            (Nothing, _) -> (path, Nothing)
            (Just '|', EAST) -> endpoint
            (Just '|', WEST) -> endpoint
            (Just '-', NORTH) -> endpoint
            (Just '-', SOUTH) -> endpoint
            (Just mirror, _) -> getPath' (fromPos nextpos:path) nextpos (nextdir mirror dir')
          where endpoint = (path, Just (fromPos nextpos))
                nextpos = pos' .->. dir'
                nextcell = BSA.lookupMaybe bsa (fromPos nextpos)

                nextdir :: Char -> Dir -> Dir
                nextdir '\\' EAST = SOUTH
                nextdir '\\' WEST = NORTH
                nextdir '\\' SOUTH = EAST
                nextdir '\\' NORTH = WEST
                nextdir '/' EAST = NORTH
                nextdir '/' WEST = SOUTH
                nextdir '/' SOUTH = WEST
                nextdir '/' NORTH = EAST
                nextdir _   dir = dir


traverse :: Map Int (Path, Path) -> IntSet -> Maybe Int-> IntSet
traverse _ traversed Nothing = traversed
traverse dict !traversed (Just pos) | pos `IS.member` traversed = traversed
                                   | pathEndPoint left == Just pos = IS.union (pathPoints left) traversed
                                   | otherwise = traverse dict
                                        (IS.union (pathPoints right)
                                                 (traverse dict (IS.union (pathPoints left) traversed) (pathEndPoint left))
                                        )
                                        (pathEndPoint right)
                                  where Just (left, right) = M.lookup pos dict

part1 :: ByteString -> IO Integer
part1 s = do
    let bsa = BSA.makeBSarray s
    let verticalPaths = map (\p -> (BSA.intIndex bsa p, (getPath bsa (toPos p) NORTH, getPath bsa (toPos p) SOUTH))) (BSA.elemIndices '|' bsa)
    let horizontalPaths = map (\p -> (BSA.intIndex bsa p, (getPath bsa (toPos p) WEST, getPath bsa (toPos p) EAST))) (BSA.elemIndices '-' bsa)
    let dict = M.fromList (verticalPaths <> horizontalPaths)
    return . toInteger $ numEnergized bsa dict (0,-1) EAST

deep :: NFData a => a -> a
deep a = deepseq a a

part2 :: ByteString -> IO Integer
part2 s = do
    let bsa = BSA.makeBSarray s
    let verticalPaths = map (\p -> (BSA.intIndex bsa p, (getPath bsa (toPos p) NORTH, getPath bsa (toPos p) SOUTH))) (BSA.elemIndices '|' bsa)
    let horizontalPaths = map (\p -> (BSA.intIndex bsa p, (getPath bsa (toPos p) WEST, getPath bsa (toPos p) EAST))) (BSA.elemIndices '-' bsa)
    let !dict = M.fromList (verticalPaths <> horizontalPaths)
    let (numrows, numcols) = (BSA.rows bsa, BSA.cols bsa)
    let l1 = [maximum (doEdge bsa dict EAST (,-1) numrows)]
    let l2 = [maximum (doEdge bsa dict WEST (,numrows) numrows)]
    let l3 = [maximum (doEdge bsa dict SOUTH (-1,) numcols)]
    let l4 = [maximum (doEdge bsa dict NORTH (numcols,) numcols)]
    return  . toInteger
            . maximum
            $ l1 <> l2 <> l3 <> l4

numEnergized :: BSArray -> Map Int (Path, Path) -> (Int, Int) -> Dir -> Int
numEnergized bsa dict p d= let firstPath = getPath bsa (Pos p) d
    in IS.size $ traverse dict (IS.delete (BSA.intIndex bsa p) (pathPoints firstPath)) (pathEndPoint firstPath)

doEdge :: BSArray -> Map Int (Path, Path) -> Dir -> (Int -> (Int,Int)) -> Int -> [Int]
doEdge bsa dict d tf l = map ((\p -> numEnergized bsa dict p d) . tf) [0..l-1]