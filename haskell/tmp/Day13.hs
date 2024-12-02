{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# LANGUAGE BangPatterns #-}

module Day13 (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 ( Parser )
import qualified Data.Attoparsec.ByteString.Char8 as AP
-- import Debug.Trace ( trace )

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import Data.Bits (shiftL, xor, popCount )
import Data.Either (fromRight)
import Data.Maybe (fromJust)

runex :: RunMe
runex =
  runMeByteString
    "Day 13 - example"
    (return example)
    part1
    (Just 405)
    part2
    (Just 400)

runme :: RunMe
runme =
  runMeByteString
    "Day 13: Point of Incidence"
    (readInpByteSTring "day13.txt")
    part1
    (Just 34911)
    part2
    (Just 33183)


example :: ByteString
example =
  [r|#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
|]

--- parsing ... 
type Mirror = [Int]
type MirrorAspects = (Mirror, Mirror)


cell ::Parser Int
cell = do
  c <- AP.char '#' <|> AP.char '.'
  case c of
    '#' ->  return 1
    '.' ->  return 0

mirrorRow :: Int -> Parser Int
mirrorRow n = do
  c <- cell
  let v = (n `shiftL` 1) + c
  mirrorRow v <|> return v


block :: Parser [ByteString]
block = many ( AP.takeWhile1 (AP.inClass ".#") <* AP.char '\n')

mirrorints :: Parser MirrorAspects
mirrorints = do
  b <- block
  let p = fromRight (error "can't parse mirror row") . AP.parseOnly (mirrorRow 0)
  return (map p b ,map p (B.transpose b))

parseInput :: Parser [MirrorAspects]
parseInput = do
  m <- mirrorints
  l <- AP.many' (AP.endOfLine *> mirrorints)
  return (m:l)

parse' :: Parser  b -> ByteString -> b
parse' p s = either (error . show) id  $ AP.parseOnly (p <* AP.endOfInput) s

--- parsing ^^ 

foldMirror :: [Int] -> [Int] -> Maybe Int
foldMirror _ [] = Nothing
foldMirror [] (y:ys) = foldMirror [y] ys 
foldMirror  bs  (y:ys) = if and $ zipWith (==) bs (y:ys) then Just (length bs)
                         else foldMirror (y:bs) ys

foldMirror' :: [Int] -> [Int] -> Maybe Int
foldMirror' _ [] = Nothing
foldMirror' [] (y:ys) = foldMirror' [y] ys 
foldMirror' bs (y:ys) = 
      case findwhile 0 accbitdiffs  of 
          1 -> Just (length bs)
          _ -> foldMirror' (y:bs) ys 

  where accbitdiffs = scanl1 (+) $ zipWith (\a b -> popCount (xor a b )) bs (y:ys) 
        findwhile current [] = current
        findwhile _ (x:xs) | x > 1 = 2 
                           | otherwise = findwhile x xs 

type Folder = ([Int] -> [Int] -> Maybe Int)

mirrorScore ::  Folder -> MirrorAspects -> Int
mirrorScore folder (a1,a2) = case folder [] a1 of 
    (Just r') -> r' * 100
    Nothing -> fromJust (folder [] a2)


---

part1 :: ByteString -> IO Integer
part1 s = do 
  let !mirrors = parse' parseInput s 
  return . toInteger
         . sum 
         . map (mirrorScore foldMirror)
         $ mirrors

part2 :: ByteString -> IO Integer
part2 s = do 
  let !mirrors = parse' parseInput s 
  return . toInteger
         . sum 
         . map (mirrorScore foldMirror')
         $ mirrors