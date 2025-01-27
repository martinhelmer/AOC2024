{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day06 (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString)

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import PosDir (Pos(..), Loc, Dir(..), rr, step, loc, pos2int, loc2int)

import qualified BSArray as BSA
import BSArray (BSArray)
import Data.Maybe (fromJust, isJust)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.IntSet as IS 
import Data.Vector (Vector)
import qualified Data.Vector as V 
import Data.List (sort)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (a:_) = Just a

flipTuple (a,b) = (b,a)

example :: ByteString
example =
  [r|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...|]

runex :: RunMe
runex =
  runMeByteString
    "Day 06 - example"
    (return example)
    part1
    (Just 41)
    part2
    (Just 6)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 6: Guard Gallivant / hop"
    (readInpByteSTring "day06.txt")
    part1
    (Just 5162)
    part2
    (Just 1909)

---

data Env = Env BSArray !(Vector [Int]) !(Vector [Int])

type Visited = IS.IntSet

vismember :: Pos -> IS.IntSet -> Bool
vismember p s = (pos2int p) `IS.member` s 

dobigstep :: Env -> Maybe Loc -> Pos -> Maybe Pos
dobigstep env = dostep env True 

dosmallstep :: Env -> Maybe Loc -> Pos -> Maybe Pos
dosmallstep env = dostep env False 

dostep :: Env
          -> Bool        -- small / big step 
          -> Maybe Loc   -- blocked position 
          -> Pos         -- step from here 
          -> Maybe Pos   
dostep env@(Env bsa _ _) many blocker p@(Pos location direction) = if Just l' == blocker then  rr' else  case stpc of
                        Nothing -> Nothing
                        Just '#' -> rr'
                        _ -> if many then hopstep env blocker p else Just stepforward
    where stepforward@(Pos l' _) = step p
          stpc = BSA.lookupMaybe bsa l'
          rr' =  Just (Pos location (rr direction))

hopstep :: Env -> Maybe Loc -> Pos -> Maybe Pos 
hopstep (Env _ rowdata coldata) blocker (Pos (row, col) direction) = 
        case direction of 
          NORTH -> moveAlongCol lastLowest
          SOUTH -> moveAlongCol firstHighest
          EAST -> moveAlongRow firstHighest
          WEST -> moveAlongRow lastLowest

        where moveAlongCol f = (\newrow -> Pos (newrow, col) direction) <$> f row (mixwithblocker col (coldata V.! col) blocker)
              moveAlongRow f = (\newcol -> Pos (row, newcol) direction) <$> f col (mixwithblocker row (rowdata V.! row) $ (\(a,b) -> (b,a)) <$> blocker)

moveAlongCol' = moveAlong id 
moveAlongRow' = moveAlong flipTuple 

moveAlong :: ((Int, Int) -> (Int, Int)) 
              -> (Int -> [Int] -> Maybe Int) 
              -> (Int, Int)
              -> Maybe (Int, Int) 
              -> Dir
              -> Vector [Int] 
              -> Maybe Pos
moveAlong flipper f t blocker direction v = 
    let (row,col) = flipper t in 
    (\newv -> Pos (flipper (row, newv)) direction) <$> f col (mixwithblocker row (v V.! row) $ flipper <$> blocker)


mixwithblocker :: Int -> [Int] -> Maybe Loc -> [Int]
mixwithblocker _ l Nothing = l
mixwithblocker col l (Just (r,c)) = if col == c then sort (r:l) else l 

lastLowest :: Int -> [Int] -> Maybe Int
lastLowest n l | null l = Nothing
               | otherwise =  (+1) <$> lastlowest' Nothing n l
    where lastlowest' sofar _ [] = sofar 
          lastlowest' sofar n (x:xs) 
                        | x >= n = sofar 
                        | otherwise = lastlowest' (Just x) n xs 

firstHighest :: Int -> [Int] -> Maybe Int
firstHighest n l = (\x -> x-1) <$> (safeHead . dropWhile (<= n) $ l)

getEnv :: BSArray -> Env
getEnv bsa  = 
    let hashtags = BSA.elemIndices '#' bsa
        dat elems =  V.map sort $ V.accum (flip (:)) (V.replicate (BSA.cols bsa) []) elems  
    in Env bsa (dat hashtags) (dat (map flipTuple hashtags))

part1 :: ByteString -> IO Integer
part1 s  = do
    let bsa = BSA.makeBSarray s
        env = getEnv bsa
    let sp = Pos (fromJust $ BSA.elemIndex bsa '^') NORTH
    return (toInteger . length . nubOrd . map (\(Just (Pos p _)) -> p) . takeWhile (isJust) $ iterate (>>= (dosmallstep env Nothing )) (Just sp) )

hasLoop :: Env -> Visited -> Pos -> Loc  -> Bool
hasLoop env v sp l =  hasLoop' v (Just sp)
    where
        hasLoop' _ Nothing = False
        hasLoop' v' (Just p') = p' `vismember` v' || hasLoop' ( (pos2int p') `IS.insert` v' ) (dobigstep env (Just l) p')


martin :: Env -> Int -> IS.IntSet -> Visited-> Pos -> Int
martin env accumblocks vloc visited currentpos =
    case  dosmallstep env  Nothing currentpos of
            Nothing -> accumblocks
            (Just nextp) ->
                martin
                    env
                    (let l = loc nextp in if not (IS.member (loc2int l) vloc) && hasLoop env visited currentpos l then accumblocks + 1  else accumblocks)
                    ((loc2int . loc $ currentpos) `IS.insert` vloc)
                    ((pos2int currentpos) `IS.insert` visited)
                    nextp

part2 :: ByteString -> IO Integer
part2 s = do
    let bsa = BSA.makeBSarray s
        env = getEnv bsa 
    let sp = Pos (fromJust $ BSA.elemIndex bsa '^') NORTH

    return ( toInteger $ martin env 0 IS.empty IS.empty sp )
