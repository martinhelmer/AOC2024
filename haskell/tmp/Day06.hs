{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day06 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  decimal,
  isDigit,
  string,
  endOfInput,
  endOfLine,
  many1,
  parseOnly,
  skipSpace,
 )
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import Data.Either (fromRight)
import Data.Maybe (fromJust)

example :: ByteString
example =
  [r|Time:      7  15   30
Distance:  9  40  200
|]

runex :: RunMe
runex =
  runMeByteString
    "Day x - example"
    (return example)
    part1
    Nothing
    part2
    Nothing

runme :: RunMe
runme =
  runMeByteString
    "Day 6: Wait For It"
    (readInpByteSTring "day06.txt")
    part1
    (Just 3317888)
    part2
    (Just 24655068)

--- Parse
type Time = Int
type Distance = Int

parseInput :: (ByteString -> Parser [Int]) -> Parser [(Int,Int)]
parseInput rowparser = zip <$> rowparser "Time:"  <*>  rowparser "Distance:"

parseRow :: ByteString -> Parser [Int]
parseRow leadIn = string leadIn *> many1 (skipSpace *> decimal) <* (endOfLine <|> endOfInput)

parseRow2 :: ByteString -> Parser [Int]
parseRow2 leadIn =
  string leadIn
  *> (toIntList <$> many1 ( skipSpace *> AP.takeWhile1 isDigit ))
  <* (endOfLine <|> endOfInput)
  where toIntList = flip (:) [] . fst . fromJust . B.readInt  . B.concat

parse :: (ByteString -> Parser [Int]) -> ByteString -> [(Time, Distance)]
parse rowparser s = fromRight undefined $ parseOnly (parseInput rowparser <* endOfInput ) s

---

numWays :: Num a => (a, a) -> a
numWays (f,t) = t-f+1

winInterval :: (Integral a1, Integral b2, Integral a, Integral b) => (a1, b2) -> (a, b)
winInterval (t, d) = (truncate s1 + 1, truncate s2 + if snd (properFraction s2::(Int,Double)) > 0.0 then  0 else 1)
  where t' =  fromIntegral t :: Double
        d' =  fromIntegral d :: Double
        q = sqrt ( t' * t' - 4 * d')
        s1 = (t' - q) / 2
        s2 = (t' + q) / 2

intervalProd :: [(Time, Distance)] -> Integer
intervalProd = product . map (numWays . winInterval)

part1 :: ByteString -> IO Integer
part1 = return . intervalProd . parse parseRow

part2 :: ByteString -> IO Integer
part2  = return . intervalProd . parse parseRow2
