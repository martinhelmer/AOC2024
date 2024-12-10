{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day09 (runme, runex) where

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
import AOCHelper (readInpByteSTring, Pos, Dir, tp)
import qualified BSArray as BSA
import Data.Char (digitToInt)
import Data.List (foldl')
import qualified Data.Set as S 

runex :: RunMe
runex =
  runMeByteString
    "Day 09 - example"
    (return example)
    part1
    (Just 1928)
    part2
    (Just 2858)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 9: Disk Fragmenter ---"
    (readInpByteSTring "day09.txt")
    part1
    (Just 6471961544878)
    part2
    (Just 6511178035564)

example :: ByteString
example =
  [r|2333133121414131402|]

parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

---
type Files = [Int]
type Gaps = [Int]
data P = Num Int | Gup deriving (Show, Eq )


readinp :: String -> Int -> [P]
readinp [] _ = []
readinp "\n" _ = []
readinp  [f] counter  = (replicate (digitToInt f) (Num counter))
readinp  [f, '\n'] counter  = (replicate (digitToInt f) (Num counter))
readinp  (f:g:xs) counter  = (replicate (digitToInt f) (Num counter)) ++ (replicate (digitToInt g) Gup) ++ readinp xs (counter +1)

foldit :: [(Int, P)] -> [(Int,Int)] -> [Int]
foldit ((ix, next):xs) ft@((ix2, n2):t)
        | ix > ix2 = []
        | otherwise = case next of
                    Gup ->  n2 : foldit xs t
                    Num n -> n : foldit xs ft

part1 :: ByteString -> IO Integer
part1 s = do
    let q = zip [0..] (readinp (BS.unpack s) 0)
    let rev = map (\(i, Num n) -> (i, n)) . reverse $ filter (\(_, q) -> q /= Gup) q
    let s = foldit q rev
    (return . toInteger . sum) (zipWith ((*)) [0..] s)

-- 
type ID = Int
type Size = Int

data P2 = File !ID !Size | Gap !Size deriving (Show, Eq, Ord )

isFile :: P2 -> Bool
isFile (File _ _) = True
isFile (Gap _) = False

size :: P2 -> Size
size (File _ s) = s
size (Gap s) = s


readinp2 :: String -> Int -> [P2]
readinp2 [] _ = []
readinp2 "\n" _ = []
readinp2  [f] counter  = [File counter (digitToInt f)]
readinp2  [f, '\n'] counter  = readinp2 [f] counter
readinp2  (f:g:xs) counter  = File counter (digitToInt f) : Gap (digitToInt g) : readinp2 xs (counter +1)

fit :: [P2] -> P2 -> [P2]
fit l@(x@(File _ _):xs) file
        | file == x = l
        | otherwise = x:(fit xs file)
fit (x@(Gap gapsize):xs) file
        | gapsize < size file = x:(fit xs file)
        | gapsize == size file = file :  xs 
        | otherwise = file:  (Gap (gapsize - size file):xs)


fitall :: [P2] -> [P2] -> [P2]
fitall = foldl' fit

checksum' :: [P2] -> S.Set P2 -> Int -> Int 
checksum' [] _ _ = 0 
checksum' (x:xs) v counter = (this x) + checksum' xs (S.insert x v) (counter + size x)
    where this (Gap _)= 0 
          this f@(File i s) = if S.member f v then 0 else  i *  sum [counter .. (counter + s -1)]

part2 :: ByteString -> IO Integer
part2 s = do
    let stuff = readinp2 (BS.unpack s) 0
    let fromright = (reverse . filter isFile) stuff
    let all = fitall stuff fromright
    return  (toInteger (checksum' all S.empty 0))
