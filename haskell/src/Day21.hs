{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BangPatterns #-}

module Day21 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import qualified Control.Parallel.Strategies as S
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
 )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import PosDir (Dir)
import qualified BSArray as BSA
import Data.Foldable (foldl')
import Data.Char (ord)
import Data.Function.Memoize 
import qualified Data.MemoTrie as MT 

example :: ByteString
example =
  [r|029A
980A
179A
456A
379A
|]

runex :: RunMe
runex =
  runMeByteString
    "Day 21 - example"
    (return example)
    part1
    (Just 126384)
    part2
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 21: Keypad Conundrum ---"
    (readInpByteSTring "day21.txt")
    part1
    (Just 136780)
    part2
    (Just 167538833832712)

type NF = Char -> [Char] -> [Char]
---

nf1 :: Foldable t => Char -> t Char -> [Char]
nf1 c v = filter (`notElem` v) n
        where n = case c of
                    'A' -> ['0', '3']
                    '0' -> ['2', 'A']
                    '1' -> ['4', '2']
                    '2' -> ['1', '0', '5', '3']
                    '3' -> ['2', '6', 'A']
                    '4' -> ['7', '5', '1']
                    '5' -> ['8', '4', '2', '6']
                    '6' -> ['5', '9', '3']
                    '7' -> ['4', '8']
                    '8' -> ['7', '5', '9']
                    '9' -> ['8', '6']

d1 :: (Char, Char) -> Char
d1 (a,b) = case (a,b) of
        ('A','0') -> '<'
        ('0','A') -> '>'
        ('0', '2') -> '^'
        ('2', '0') -> 'v'
        ('A','3') -> '^'
        ('3','A') -> 'v'
        (f,t) -> case (ord t - ord f) of
                    1 -> '>'
                    -1 -> '<'
                    -3 -> 'v'
                    3 -> '^'

nf2 :: Foldable t => Char -> t Char -> [Char]
nf2 c v = filter (`notElem` v) n
        where n = case c of
                    'A' -> ['^', '>']
                    '^' -> ['A', 'v']
                    '>' -> ['A', 'v']
                    'v' -> ['<', '^', '>']
                    '<' -> ['v']

d2 :: (Char, Char) -> Char
d2 (a,b) = case (a,b) of
        ('A','^') -> '<'
        ('A','>') -> 'v'
        ('^', 'A') -> '>'
        ('^', 'v') -> 'v'
        ('<','v') -> '>'
        ('v', d) -> d
        ('>', 'A') -> '^'
        ('>', 'v') -> '<'
        (e,f) -> error (show (e,f))

shortestPath :: NF -> Char -> Char -> [[Char]]
shortestPath nf f to = sp 99 [f] [f]
  where
    sp bestsofar tl visited
                | length tl > bestsofar = []
                | from == to = [tl]
                | otherwise = fst $ foldl' go ([], bestsofar) (nf from visited)
        where from = head tl
              go (l, b) next | null fs' = (l,b)
                             | (length (head fs')) < b = (fs', (length (head fs')) )
                             | otherwise = (l ++ fs' ,b)
                              where fs' = sp b (next:tl) (from:visited)

fs2 :: Char -> Char -> [[Char]]
fs2 =  findshortest' True nf2 d2 

findshortest' :: Bool -> NF -> ((Char, Char)-> Char) -> Char -> Char -> [[Char]]
findshortest' deep nf d f t =
    let f' = shortestPath nf f t
        shortest = map ((++ "A") . (\s -> zipWith (curry d) s (tail s) ) . reverse) f'
    in if not deep then shortest else
            let godeep = map (\s -> (length $ fsFromString False 6 3 s, s)) shortest
            in [snd . minimum $ godeep]

fs :: Bool -> Int -> Int -> Char -> Char -> [Char]
fs deep st n f t
    | n == 0 = head $ findshortest' False nf2 d2 f t
    | otherwise =
            let fssl = map (fsFromString deep st (n-1)) $ findshortest' deep (if n == st then nf1 else nf2) (if n == st then d1 else d2) f t
                minl = minimum (map length fssl)
            in head $ filter (\s -> length s == minl) $ fssl


fsFromString :: Bool -> Int -> Int -> [Char] -> [Char]
fsFromString deep st n s = concatMap (uncurry (fs deep st n)) $ zip ('A':s) s

--- 
digitPadPaths :: [Char] -> [Char]
digitPadPaths s = concat $ zipWith (fs') ('A':s) s
    where fs' f t=  head $ findshortest' True nf1 d1 f t

padLength ::  Int -> [Char] -> Int
padLength n s = let p =  digitPadPaths s  
                    !l = (zipWith ((expandedLength n)) ('A':p) p) `S.using` S.parList S.rseq 
                in sum l 

expandedLength :: Int -> Char -> Char -> Int
expandedLength = memoize3 expandedLength'

expandedLength' :: Int -> Char -> Char -> Int
expandedLength' 0 _ _ = 1
expandedLength' n f t = sum . map (uncurry (expandedLength (n - 1))) $ let p =  head $ fs2 f t in (zip ('A':p) p)

--- 

numcode :: String -> Int
numcode s = read (init s)

part1 :: ByteString -> IO Integer
part1 s = do
    let codes = map (BS.unpack) $ BS.lines s
    -- print  (map (\s -> (length $ fs' 2 s, numcode s)) codes)
    return . toInteger . sum $ (map (\code -> (padLength 2 code) * (numcode code)) codes)

part2 :: ByteString -> IO Integer
part2 s = do
    let codes = map (BS.unpack) $ BS.lines s
    let !ls = (map (\code -> (padLength 25 code) * (numcode code)) codes) `S.using` S.parListChunk 1 S.rseq 
    return . toInteger . sum $ ls
