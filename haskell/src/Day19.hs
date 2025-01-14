{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE TupleSections #-}

module Day19 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as AP
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
  sepBy,
  letter_ascii
 )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, Pos, Dir, tp)
import qualified BSArray as BSA
import qualified Data.HashSet as S
import qualified Data.Map as M
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)
import Data.List (foldl')

--
runex :: RunMe
runex =
  runMeByteString
    "Day 19 - example"
    (return example)
    part1
    (Just 6)
    part2
    (Just 16)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 19: Linen Layout ---"
    (readInpByteSTring "day19.txt.orig")
    part1
    (Just 236)
    part2
    (Just 643685981770598)

---
parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

example :: ByteString
example =
  [r|r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
|]


parseTowels :: Parser Towels
parseTowels = do
    words' <- (many1 letter_ascii) `sepBy` ", "
    return $ Towels (maximum . map length $ words') (S.fromList $ (map BS.pack) words')

readInput s = let (x:y:xs) = BS.lines s in (parse' parseTowels x, xs)

type TowelSet = S.Set ByteString

data Towels = Towels { maxlen :: Int
                     , towelset :: TowelSet} deriving (Eq, Show)

type Visited = M.Map ByteString Int

hasPattern :: Towels -> ByteString -> Bool
hasPattern t@(Towels ml set) bs =
        BS.null bs || any (\n -> (S.member (BS.take n bs) set) && hasPattern t (BS.drop n bs)) [1..(min ml (BS.length bs))]

ways :: Visited -> Towels -> ByteString -> (Int, Visited)
ways visited t@(Towels ml set) bs = maybe go (,visited) (M.lookup bs visited)
        where   go :: (Int, Visited)
                go | BS.null bs = (1, visited)
                   | otherwise = 
                        let (s', v'') =  foldl' gg (0, visited) [1..(min ml (BS.length bs))] 
                        in (s', M.insert bs s' v'')
                
                        where gg (s, v) n 
                                    | not (S.member (BS.take n bs) set)  = (s,v)
                                    | otherwise = let (s', v' ) = ways v t (BS.drop n bs) in (s + s', v')

part1 :: ByteString -> IO Integer
part1  s = do
    let (towels, patterns) = readInput s
    return . toInteger . length . filter (hasPattern towels) $ patterns


part2 :: ByteString -> IO Integer
part2  s = do
    let (towels, patterns) = readInput s
    return . toInteger . sum $ map (fst . ways (M.empty) towels) patterns