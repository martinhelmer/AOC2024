{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day01 (runme, runex) where

import Text.RawString.QQ
import AOCHelper
import Data.List (transpose, sort)
import qualified Data.IntMap as M
import Data.Maybe ( fromMaybe, fromJust )

import RunUtil (runMeString, RunMe)
runme :: RunMe
runme = runMeString "--- Day 1: Historian Hysteria ---"
              (readInp "day01.txt")
              (fmap toInteger . part1)
              (Just 2430334)
              (fmap toInteger . part2)
              (Just 28786472)
----------------------------------    

runex :: RunMe
runex =
    runMeString
        "Day 1 - example"
        (return example)
        (fmap toInteger . part1)
        (Just 11)
        (fmap toInteger . part2)
        (Just 31)

example :: String
example = [r|3   4
4   3
2   5
1   3
3   9
3   3|]

----------------------------


frequencies :: [Int] -> M.IntMap Int
frequencies = foldr (\x m -> M.insertWith (+) x 1 m) M.empty

unzip' :: [[Int]] -> ([Int], [Int])
unzip' [] = ([],[])
unzip' (x:xs) = (a:l1, b:l2)
    where (l1,l2) = unzip' xs
          [a,b] = x

sTol1l2 :: String -> ([Int], [Int])
sTol1l2 = unzip' . map (map read . words) . lines

part1 :: String -> IO Int
part1 s = do
    let (l1,l2) = sTol1l2 s
    return $ sum $ zipWith (\a b -> abs (a-b)) (sort l1) (sort l2)

part2 :: String -> IO Int
part2 s = do
    let (l1,l2) = sTol1l2 s
    let counts = map (fromMaybe 0 . (`M.lookup` frequencies l2)) l1
    return $ sum $ zipWith (*) l1 counts
