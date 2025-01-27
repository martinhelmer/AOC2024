{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day05 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  char,
  sepBy,
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


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, Pos, Dir, tp)
import qualified BSArray as BSA
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.List (sortBy)


runex :: RunMe
runex =
  runMeByteString
    "Day 05 - example"
    (return example)
    part1
    (Just 143)
    part2
    (Just 123)

runme :: RunMe
runme =
  runMeByteString
    "-- Day 5: Print Queue ---"
    (readInpByteSTring "day05.txt")
    part1
    (Just 5064)
    part2
    (Just 5152)

---
example :: ByteString
example =
  [r|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47|]

data Problem = Problem { omap :: OMap, rows :: [[Int]] } deriving (Show)

parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

parsePair :: Parser (Int,Int)
parsePair = (,) <$> decimal <* char '|' <*> decimal <* endOfLine

parseNums :: Parser [Int]
parseNums = (decimal `sepBy` ",")

parsePairs :: Parser [(Int,Int)]
parsePairs = many parsePair

parseNumss :: Parser [[Int]]
parseNumss = parseNums `sepBy` "\n"


parse :: ByteString -> Problem
parse = parse' ((Problem . ltmap ) <$> parsePairs <* endOfLine <*> parseNumss)

type OMap = M.IntMap S.IntSet

ltmap :: [(Int, Int)] -> OMap
ltmap = foldr (\(lt, gt) m -> M.insertWith S.union lt (S.singleton gt) m) M.empty

pairIsOrdered :: OMap -> Int -> Int ->  Bool
pairIsOrdered m a b = let s = M.lookup b m in maybe True (S.notMember a) s

lineIsOrded :: OMap -> [Int] -> Bool
lineIsOrded om l = all (uncurry $ pairIsOrdered om) (zip l (tail l))

sort' :: OMap -> [Int] -> [Int]
sort' om = sortBy (\a b -> if pairIsOrdered om a b  then LT else GT )

middleitem :: [a] -> a
middleitem l = (!! max 0 (length l `div` 2)) l 

part1 :: ByteString -> IO Integer
part1 s = do
    let (Problem omap' mlist')= parse s
    let mlist = filter (not . null) mlist'
    -- print (filter  (lineIsOrded omap') $ mlist)
    return  (toInteger . sum .  map middleitem . filter  (lineIsOrded omap') $ mlist)

part2 :: ByteString -> IO Integer
part2 s = do
    let (Problem omap' mlist')= parse s
    let mlist = filter (not . null) mlist'
    return  (toInteger
            . sum
            . map middleitem
            . map (sort' omap')
            . filter  (not . lineIsOrded omap') $ mlist)
