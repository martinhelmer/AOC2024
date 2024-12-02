{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day02 (runme, runex) where

import Text.RawString.QQ

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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List ( inits, tails )
import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, Pos, Dir, tp)
import qualified BSArray as BSA

example :: ByteString
example =
  [r|7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9|]

runex :: RunMe
runex =
  runMeByteString
    "Day 2 - example"
    (return example)
    part1
    (Just 2)
    part2
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 2: Red-Nosed Reports ---."
    (readInpByteSTring "day02.txt")
    part1
    (Just 326)
    part2
    (Just 381)

---
parse :: ByteString -> [[Int]]
parse = map (map (fst . fromJust . BS.readInt) . BS.words) . BS.lines

isSafe :: [Int] -> Bool
isSafe report = isSafe' Nothing (zipWith  (-) (tail report) report)       -- taking the diffs
  where isSafe' Nothing (x:xs) = isSafe' (Just x) xs
        isSafe' (Just s) [] = rangeok s 
        isSafe' (Just s) (x:xs) = (rangeok s) && (s * x > 0 ) && isSafe' (Just x) xs
        rangeok s = s /= 0 && (abs s) <= 3


isSafe2 :: [Int] -> Bool
isSafe2 report = any isSafe $ report:(zipWith ((++)) (inits report) (tail $ tails report))

part1 :: ByteString -> IO Integer
part1 s = do
  return $ toInteger . length . filter isSafe $ (parse s)

part2 :: ByteString -> IO Integer
part2  s = do
  return $ toInteger . length . filter isSafe2 $ (parse s)

