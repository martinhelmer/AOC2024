{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module Day09 (runme, runex) where

import Text.RawString.QQ

import Data.Attoparsec.ByteString.Char8 (
  Parser,
  decimal,
  endOfInput,
  space,
  signed,
  sepBy1',
 )
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)

example :: ByteString
example =
  [r|0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45|]

runex :: RunMe
runex =
  runMeByteString
    "Day 09 - example"
    (return example)
    part1
    Nothing
    part2
    Nothing

runme :: RunMe
runme =
  runMeByteString
    "Day 9: Mirage Maintenance"
    (readInpByteSTring "day09.txt")
    part1
    (Just 1898776583)
    part2
    (Just 1100)

---
parse' :: Parser  b -> ByteString -> b
parse' p s =  let !q = either (error . show) id  $ AP.parseOnly (p <* endOfInput) s in q

part1 :: ByteString -> IO Integer
part1 s = do
  let numbers =map (parse' (signed decimal `sepBy1'` space)) (B.lines s) :: [[Integer]]
  return . sum $ map next numbers

part2 :: ByteString -> IO Integer
part2 s = do
  let numbers =map (parse' (signed decimal `sepBy1'` space)) (B.lines s) :: [[Integer]]
  return . sum $ map (next . reverse) numbers

next :: (Eq c, Num c) => [c] -> c
next xs = if all (==0) xs then 0 else last xs + next (diffs xs)
      where diffs l = zipWith (-) (tail l) l

