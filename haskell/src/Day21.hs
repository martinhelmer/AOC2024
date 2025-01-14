{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day21 (runme, runex) where

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
 )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import PosDir (Dir)
import qualified BSArray as BSA
import Data.Foldable (foldl')
import Data.Char (ord)

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
    (Nothing)

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

findshortest ::Int -> [Char] -> NF -> [Char] -> Char ->  [[Char]]
findshortest bestsofar t nf visited to
                | length t > bestsofar = []
                | from == to = [t]
                | otherwise = fst q

        where from = head t
              q = foldl' go ([], bestsofar) (nf from visited)
              go (l, b) next  =
                     let fs =  findshortest b (next:t) nf (from:visited) to
                     in if (null fs) then (l,b) else if (length (head fs)) < b then (fs, (length (head fs)) ) else (l ++ fs ,b)

findshortest' :: NF -> ((Char, Char)-> Char) -> Char -> Char -> [[Char]]
findshortest' nf d f t =
    let f' = findshortest 99 [f] nf [f] t
    in map ((++ "A") . (\s -> zipWith (curry d) s (tail s) ) . reverse) f'

fs st n f t
    | n == 0 = head $ findshortest' nf2 d2 f t
    | otherwise =
            let fssl = map fseq $ findshortest' (if n == st then nf1 else nf2) (if n == st then d1 else d2) f t
                fseq s = concatMap (uncurry (fs st (n - 1))) (zip ('A':s) s)
                minl = minimum (map length fssl)
            in head $ filter (\s -> length s == minl) $ fssl

fs' n s = concatMap (\(f,t) -> fs n n f t) $ zip ('A':s) s

numcode :: String -> Int
numcode s = read (init s)

part1 :: ByteString -> IO Integer
part1 s = do
    let codes = map (BS.unpack) $ BS.lines s
    -- print  (map (\s -> (length $ fs' 2 s, numcode s)) codes)
    return . toInteger . sum $ (map (\s -> length (fs' 2 s) * (numcode s)) codes)

part2 :: ByteString -> IO Integer
part2 s = do
    let codes = map (BS.unpack) $ BS.lines s
    let c = head codes 
    let ls =  ( map (\n -> length (fs' n c)) [2,3,4,5,6,7,8,9,10])
    -- print (ls)
    -- print (map (\(a,b) -> (toRational a)/(toRational b)) $ zip (drop 2 $ ls) ls )
    -- print ((fs' 5 c))
    return 0