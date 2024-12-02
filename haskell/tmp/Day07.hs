{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Day07 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  decimal,
  endOfLine,
  decimal,
  space, 
 )
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (foldl1', sortBy, sortOn )
import qualified Data.Map.Strict as Map

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import Data.Ord (comparing, Down (Down))

example :: ByteString
example =
  [r|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
|]

runex :: RunMe
runex =
  runMeByteString
    "example: Day 7: Camel Cards"
    (return example)
    part1
    (Just 6440)
    part2
    (Just 5905)

runme :: RunMe
runme =
  runMeByteString
    "Day 7: Camel Cards"
    (readInpByteSTring "day07.txt")
    part1
    (Just 251106089)
    part2
    (Just 249620106)

---
data Card = Card (Map.Map Char Int) ByteString Int deriving (Show)

parseCard :: Parser Card
parseCard = do
  w <- AP.take 5
  _ <- space
  i <- decimal
  _ <- endOfLine
  let counter =  countEach (B.unpack w )
  return (Card counter w i )

parse' :: Parser  b -> ByteString -> b
parse' p s = either (error . show) id  $ AP.parseOnly (p <* AP.endOfInput) s
-- 

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (comparing Down)

countEach :: Ord a => [a] -> Map.Map a Int
countEach ax = Map.fromListWith (+) $ map (,1) ax

sortvalue' ::[Int] -> Map.Map Char Int-> ByteString -> Int
sortvalue' cardgroups ordermap s = l1 * 14^(5::Int) + l2
  where l1 = foldl1' (\a v -> a*6+v) cardgroups * 6 ^ (5-length cardgroups)
        l2 = B.foldl (\acc ch -> acc*14 +(ordermap Map.! ch) ) 0 s

calculate :: Ord b => (Card -> b) -> ByteString -> Integer
calculate sortf s = 
    let cards = sortOn sortf $ parse' (many parseCard) s
    in (toInteger . sum) (zipWith (\(Card _ _ p) rank-> p * rank  ) cards [1..])

mkorder :: (Ord k, Num a, Enum a) => [k] -> Map.Map k a
mkorder s = Map.fromList . zip (reverse s) $ [0..]
-- 
sortvalue1 :: Card -> Int
sortvalue1 (Card counter bs _) = sortvalue' counts (mkorder "AKQJT98765432") bs
 where
  counts = sortDesc (Map.elems counter)

part1 :: ByteString -> IO Integer
part1 = return . calculate sortvalue1

-- 
sortvalue2 :: Card -> Int
sortvalue2 (Card counter bs _ ) = sortvalue' counts (mkorder "AKQT98765432J") bs
      where
        counts = updatewithjacks jacks (sortDesc (Map.elems rem'))
        (jacks, rem') = Map.updateLookupWithKey (\_ _ -> Nothing) 'J' counter
        updatewithjacks _ [] = [5] -- only jacks
        updatewithjacks Nothing l = l
        updatewithjacks (Just j) (x:xs) = (x+j):xs

part2 :: ByteString -> IO Integer
part2 = return . calculate sortvalue2
