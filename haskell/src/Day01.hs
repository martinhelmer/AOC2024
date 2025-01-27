{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day01 (runme, runex) where

import Text.RawString.QQ (r)
import AOCHelper ( readInpByteSTring )
import Data.List (sort)
import qualified Data.IntMap.Strict as M
import Data.Maybe ( fromMaybe, fromJust )

import RunUtil (runMeByteString, RunMe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
runme :: RunMe
runme = runMeByteString "-- Day 1: Historian Hysteria"
              (readInpByteSTring "day01.txt")
              (fmap toInteger . part1)
              (Just 2430334)
              (fmap toInteger . part2)
              (Just 28786472)
----------------------------------    

runex :: RunMe
runex =
    runMeByteString
        "Day 1 - example"
        (return example)
        (fmap toInteger . part1)
        (Just 11)
        (fmap toInteger . part2)
        (Just 31)

example :: ByteString
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

part1 :: ByteString -> IO Int
part1 s = do
    let (l1,l2) = bsTol1l2 s
    return $ sum $ zipWith (\a b -> abs (a-b)) (sort l1) (sort l2)

bsTol1l2 :: ByteString -> ([Int], [Int])
bsTol1l2 = unzip' . map (map (fst . fromJust . BS.readInt) . BS.words) . BS.lines

part2 :: ByteString -> IO Int
part2 s = do
    let (l1,l2) = bsTol1l2 s
    let counts = map (fromMaybe 0 . (`M.lookup` frequencies l2)) l1
    return $ sum $ zipWith (*) l1 counts

-- 18 ms gain from switching from String to Bytestring