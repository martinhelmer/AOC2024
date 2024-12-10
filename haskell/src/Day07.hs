{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day07 (runme, runex) where

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
import Data.Maybe (catMaybes, mapMaybe)

runex :: RunMe
runex =
  runMeByteString
    "Day 07 - example"
    (return example)
    part1
    (Just 3749)
    part2
    (Just 11387)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 7: Bridge Repair ---"
    (readInpByteSTring "day07.txt")
    part1
    (Just 5702958180383)
    part2
    (Just 92612386119138)

---
example :: ByteString
example =
  [r|190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20|]

parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

data Record = Record !Int ![Int] deriving (Show)

value :: Record -> Int
value (Record v _)  = v 

parseRow :: Parser Record
parseRow = Record <$> decimal <* char ':' <* char ' ' <*> decimal `sepBy` " "


canbebw :: [(Int -> Int -> Maybe Int)] -> Int -> [Int] -> Bool
canbebw _ _ [] = False
canbebw _ target [x] = target == x
canbebw checks target (x:xs) =  any (\nt -> canbebw checks nt xs) $ mapMaybe (\c -> c target x) checks

subit :: Num a => a -> a -> Maybe a
subit t x  = Just (t - x)

divit :: Integral a => a -> a -> Maybe a
divit t x =
    case t `mod` x of
      0 -> Just (t `div` x)
      _ -> Nothing

striptail :: Int -> Int -> Maybe Int
striptail a b | a > 10 && (a `mod` 10 == b `mod` 10) = 
                  if b < 10 then  Just (a `div` 10) else striptail (a `div` 10) (b `div` 10)
              | otherwise = Nothing

go :: Monad m => ByteString -> (Int -> [Int] -> Bool) -> m Integer
go s canbe = do 
    let  rows =  ( (map (parse' (parseRow) ) (BS.lines s)) )
    return  . toInteger 
            . sum 
            . map value 
            . filter (\(Record i l) -> canbe i (reverse l) )
            $ rows 

part1 :: ByteString -> IO Integer
part1 s = do
  go s (canbebw [subit , divit ])

part2 :: ByteString -> IO Integer
part2 s = do
  go s (canbebw [subit , divit, striptail ])
