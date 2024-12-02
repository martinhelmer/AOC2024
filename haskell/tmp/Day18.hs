{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day18 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import qualified  Data.Attoparsec.ByteString.Char8 as AP
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
  inClass,
 )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import qualified BSArray as BSA
import PosDir

example :: ByteString
example =
  [r|R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)|]
---
runex :: RunMe
runex =
  runMeByteString
    "Day 18 - example"
    (return example)
    part1
    (Just 62 )
    part2
    (Just 952408144115)

runme :: RunMe
runme =
  runMeByteString
    "Day 18: Lavaduct Lagoon"
    (readInpByteSTring "day18.txt")
    part1
    (Just 49578)
    part2
    (Just 52885384955882)

---

direction :: Parser Dir
direction = c2d <$> AP.satisfy (inClass "RDLU0123")
    where c2d c | inClass "R0" c = EAST
                | inClass "D1" c = SOUTH 
                | inClass "L2" c = WEST 
                | inClass "U3" c = NORTH

parserow2 :: Parser Pos
parserow2 = do
  _ <- AP.take 2
  _ <- decimal ::Parser Int 
  _ <- AP.take 3 
  h <- AP.take 5 
  d <- direction
  _ <- AP.take 1
  return $ (.*->.) (read ("0x"<>BS.unpack h)) d

parserow1  :: Parser Pos
parserow1 =(flip (.*->.)) <$> direction <* AP.space <*> AP.decimal <* AP.space <* AP.take 9 

parse' :: Parser  b -> ByteString -> b
parse' p s = either (error . show) id  $ AP.parseOnly (p <* AP.endOfInput) s

---

circumference :: [Pos] -> Int
circumference l = sum $ zipWith mhdist l (drop 1 l)

shoelace :: [Pos] -> Int
shoelace l = shoelace' + circumference l `div` 2 + 1 
  where shoelace' = flip div 2 
                    . abs 
                    . sum 
                    $ zipWith det l (drop 1 l)
        det (Pos (x1,y1)) (Pos (x2,y2)) = x1 * y2 - x2 * y1

calculate :: Parser Pos -> ByteString -> Integer
calculate parser s = let corners = ((parse' parser) <$> BS.lines s) in 
      toInteger 
      . shoelace
      . scanl ((.+.)) (Pos (0,0)) $ corners

part1 :: ByteString -> IO Integer
part1 = pure . calculate parserow1 

part2 :: ByteString -> IO Integer
part2 = pure . calculate parserow2