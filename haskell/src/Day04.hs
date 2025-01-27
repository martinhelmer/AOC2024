{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day04 (runme, runex) where

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


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, Pos, Dir, tp)
import qualified BSArray as BSA
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (tails, delete, inits, sort, (\\))

example :: ByteString
example =
  [r|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|]

runex :: RunMe
runex =
  runMeByteString
    "Day x - example"
    (return example)
    part1
    (Just 18 )
    part2
    (Just 9)

runme :: RunMe
runme =
  runMeByteString
    "-- Day 4: Ceres Search ---"
    (readInpByteSTring "day04.txt")
    part1
    (Just 2462)
    part2
    (Just 1877)

---

-- sequence of relative points to select
type Selection = [(Int,Int)]

-- add 2 tuples 
(.+.):: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(.+.) (a,b) (c,d) = (a+c,b+d)

tk :: BSA.BSArray -> [Selection] -> (Int, Int) -> [String]
tk bs searchdirs p = mapMaybe ( mapM (discardX . BSA.lookupMaybe bs . (p .+.))) searchdirs
    where discardX j = if j == Just 'X' then Nothing else j

go :: [Selection] -> (String -> Bool) -> Char -> ByteString -> IO Integer
go searchdirs filt c s = do
    let bs = (BSA.makeBSarray s)
    return  ( toInteger . sum .  map (length . filter filt . (tk bs searchdirs)) $ (BSA.elemIndices c bs) )

part1 :: ByteString -> IO Integer
part1 = go ([(\ d -> take 3 $ iterate (d .+.) d) (x, y) |
               x <- [- 1 .. 1], y <- [- 1 .. 1], (x, y) /= (0, 0)])
           (=="MAS")
           'X'

part2:: ByteString -> IO Integer
part2 = go [[(-1, -1), (1,1), (1,-1), (-1,1)]]
           (\[a,b,c,d] -> ((a,b) == ('S','M') || (a,b) == ('M','S')) && ((c,d) == ('S','M') || (c,d) == ('M','S')))
           'A'

-- part3 :: Num b => p -> IO b
-- part3 s = do
--   print (head . sort . map compute $ collectsums 6 384 tl)
--   return 0


tl :: [Int]
tl = [1, 2, 3,5,7,13,17,19, 23,29, 31,37, 41,43,53,59, 61,67,71,73,79,83,89,97,101,103,107,109, 113]

collectsums :: Int -> Int -> [Int] ->[[Int]]
collectsums _ 0 _ = [[]]
collectsums mx s inp | s < 0 = []
                  | null inp = []
                  | mx <= 0 = []
                  | otherwise = concatMap (\(x:xs) -> map (x:) (collectsums (mx-1) (s-x) xs )) (init . tails $ inp)


compute :: [Int] ->  (Int, Int, Bool)
compute l = (length l, product l, not $ null (collectsums 99 384 (tl \\ l)))