{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day05 (runme, runex) where

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
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (foldl', sort )
import Data.Text (Text)
import qualified Data.Text as T

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)

example :: ByteString
example =
  [r|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|]

runex :: RunMe
runex =
  runMeByteString
    "Day 5 - example"
    (return example)
    part1
    (Just 35)
    part2
    (Just 46)

runme :: RunMe
runme =
  runMeByteString
    "Day 5: If You Give A Seed ..."
    (readInpByteSTring "day05.txt")
    part1
    (Just 389056265)
    part2
    (Just 137516820)

--- parsing
type Range = (Integer, Integer)
type Seeds = [Range] -- range - inclusive
type Maps = [SMap]
data SMap = SMap Text [(Range, Integer)] deriving (Show)
data Triple = Triple Integer Integer Integer

spacedecimal :: Parser Integer
spacedecimal = skipSpace *> decimal

-- seeds: 79 14 55 13
parseSeeds1 :: Parser Seeds
parseSeeds1 = do
  l <- "seeds:" *> many spacedecimal <* endOfLine
  return $ map (\i -> (i, i)) l

parseSeeds2 :: Parser Seeds
parseSeeds2 = do
  l <- "seeds:" *> many ( (,) <$> spacedecimal <*> spacedecimal) <* endOfLine
  return $ map (\(mst,l') -> (mst, mst+l'-1)) l

--
-- seed-to-soil map:
-- 50 98 2
-- 52 50 48
parseMap :: Parser SMap
parseMap = do
    _ <- endOfLine
    title <- T.pack . B.unpack <$> AP.takeWhile (/= ':')
    _ <- skipWhile (not . isDigit)
    ms <-  many1 mappings
    return $ SMap title (sort ms)

mappings :: Parser (Range, Integer)
mappings = do
  Triple to from len <-
    Triple
      <$> decimal
      <*> spacedecimal
      <*> spacedecimal
      <* (endOfInput <|> endOfLine)
  return ((from, from + len - 1), to)

parseInput :: Parser Seeds -> Parser (Seeds, Maps)
parseInput seedparser = (,) <$> seedparser <*> many parseMap

parse :: Parser Seeds -> ByteString -> (Seeds, Maps)
parse seedparser s =
  case parseOnly (parseInput seedparser <* endOfInput) s of
    Right (s', m) -> (s', m)
    Left _ -> undefined


applyRangeToMap :: [(Range, Integer)] -> Range-> [Range]
applyRangeToMap [] rng = [rng]                                             -- \*-*
applyRangeToMap mr@(((mst, men), mto) : ranges) rng@(st, en)
  | en < mst = [rng]                                                       -- \*-*   |-| ..
  | st > men = applyRangeToMap ranges rng                                  -- \|-| ... *-*
  | st >= mst && en <= men = [(st + offset, en + offset)]                  -- \|*-*| ...
  | st < mst = (st, mst - 1) : applyRangeToMap mr (mst, en)                -- \*-|- ...
  | st <= men && en > men =                                                -- \|-*--|-* ..
      (st + offset, men + offset) : applyRangeToMap ranges (men + 1, en)
  | otherwise = error "should not be here"
 where
  offset = mto - mst


part12 :: Parser Seeds -> ByteString -> IO Integer
part12 seedparser s = do
  let (seeds, maps) = parse seedparser s
  let done =
        foldl'
          ( \ranges (SMap _ smaps) ->
              foldr ( (<>) . applyRangeToMap smaps) [] ranges
          )
          seeds
          maps
  (return . minimum) . map fst $ done

part1 :: ByteString -> IO Integer
part1  = part12 parseSeeds1

part2 :: ByteString -> IO Integer
part2  = part12 parseSeeds2
