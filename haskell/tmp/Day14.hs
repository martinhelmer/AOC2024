
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# HLINT ignore "Use underscore" #-}

module Day14 (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM
import Data.Hashable (hash)
import Data.List (foldl', transpose, intercalate)
import Data.Maybe (mapMaybe)

import AOCHelper (readInpByteSTring)
import BSArray (makeBSarray)
import qualified BSArray as BSA
import Control.Monad (foldM)
import RunUtil (RunMe, runMeByteString)
import Debug.Trace (trace)
import Data.List.Split ( splitWhen )

example :: ByteString
example =
  [r|O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
|]

runex :: RunMe
runex =
  runMeByteString
    "Day 14 - example"
    (return example)
    part1
    (Just 136)
    part2
    (Just 64)

runme :: RunMe
runme =
  runMeByteString
    "Day 14: Parabolic Reflector Dish"
    (readInpByteSTring "day14.txt")
    part1
    (Just 109833)
    part2
    (Just 99875)

---

rollLeft' :: String -> String
rollLeft' = intercalate "#" . map go . splitWhen ('#' ==)
 where
  go :: String -> String
  go s = replicate numrocks 'O' <> replicate (length s - numrocks) '.'
   where
    numrocks = length . filter ('O' ==) $ s

rollLeftrev :: ByteString -> ByteString
rollLeftrev ss = B.intercalate "#" $ foldl' go [] (B.splitWith ('#' ==) ss)
 where
  go :: [ByteString] -> ByteString -> [ByteString]
  go a s = B.sort s:a  -- O's got to the right

rollRotRight :: BSA.BSArray -> BSA.BSArray
rollRotRight bsa = let q = map rollLeftrev (BSA.bsColsFromLeft bsa)
       in makeBSarray (B.intercalate "\n" q)

cycle' :: BSA.BSArray -> BSA.BSArray
cycle' s =  iterate rollRotRight s !! 4

rowload :: String -> Int
rowload bs = sum . mapMaybe (\(ix, ch) -> if ch == 'O' then Just ix else Nothing) $ zip (reverse [1 .. (length bs)]) bs

load :: BSA.BSArray -> Int
load = sum . map rowload . BSA.stringColsFromLeft

part1 :: ByteString -> IO Integer
part1 s = do
  let bsa = BSA.makeBSarray s
  let q = map (rowload . rollLeft') (BSA.stringColsFromLeft bsa)
  return . toInteger $ sum q

findCycle :: BSA.BSArray -> Int
findCycle bsa =
    let q =  zip [0..] (iterate cycle' bsa)
        (lead_in, cycle_restart_ix) = either id undefined (foldM f' HM.empty q )
        cycle_start_ix = lead_in + 1
        ix = (((1_000_000_000 - lead_in) `rem` (cycle_restart_ix - cycle_start_ix)) + lead_in)
    in load . snd  $ (q !! ix )
  where
    f' m (ix, bs) = let h = hash bs in case HM.lookup h m of
          Nothing -> Right (HM.insert h ix m)
          Just v ->  Left (v-1, ix)

part2 :: ByteString -> IO Integer
part2 = part2''

part2'' :: ByteString -> IO Integer
part2'' = return . toInteger . findCycle . BSA.makeBSarray

-------
rollLeft'' :: String -> String
rollLeft'' ss = intercalate "#" $ foldl' go [] (splitWhen ('#' ==) ss)
 where
  go :: [String] -> String -> [String]
  go a s = (replicate (length s - numrocks) '.' <> replicate numrocks 'O'):a
   where
    numrocks = length . filter ('O' ==) $ s

rollUpandRotRight :: [String] -> [String]
rollUpandRotRight = map rollLeft'' . transpose

part2' :: ByteString -> IO Integer
part2' s = do
    let m = lines (B.unpack s)
    -- putStrLn "-------"
    -- putStrLn (unlines m')
    return . toInteger . findCycle' $ m


cycle'' :: [String] -> [String]
cycle'' s = iterate rollUpandRotRight s !! 4

findCycle' :: [String] -> Int
findCycle' s =
    let q =  zip [0..] (iterate cycle'' s)
        (lead_in, cycle_restart_ix) = either id undefined (foldM f' HM.empty q )
        cycle_start_ix = lead_in + 1
        ix = (((1_000_000_000 - lead_in) `rem` (cycle_restart_ix - cycle_start_ix)) + lead_in)
    in load' . snd  $ (q !! ix )
  where
    f' m (ix, bs) = let h = hash bs in case HM.lookup h m of
          Nothing -> Right (HM.insert h ix m)
          Just v ->  trace (show ix) Left (v-1, ix)

load' :: [String]-> Int
load' = sum . map rowload . transpose
