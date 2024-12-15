{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day13 (runme, runex) where

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
  string,
  char,  
 )

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, Pos, Dir, tp, splitOnBs)
import qualified BSArray as BSA
import GHC.Real (Ratio((:%)))

runex :: RunMe
runex =
  runMeByteString
    "Day 13 - example"
    (return example)
    part1
    (Just 480)
    part2
    (Just 875318608908)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 13: Claw Contraption ---"
    (readInpByteSTring "day13.txt")
    part1
    (Just 29522)
    part2
    (Just 101214869433312)

---

example :: ByteString
example =
  [r|Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
|]

p b = BS.breakSubstring
solve (a1, b1, c1) (a2, b2, c2) = case ds of 
                0 -> Nothing
                _ -> Just (d1 / ds, d2 / ds)
    where d1 = b1 * c2 - b2 * c1
          d2 = c1 * a2 - c2 * a1 
          ds = a1 * b2 - a2 * b1 

solveMach ::Machine -> Int -> Maybe (Rational, Rational)
solveMach (Machine (ax, ay) (bx, by) (x,y)) error = 
        solve (fromIntegral ax, fromIntegral bx,- (fromIntegral $ x + error)) 
              (fromIntegral ay, fromIntegral by, - (fromIntegral $ y + error))

cost ::Int -> Machine -> Int 
cost e m  = case solveMach m e of 
        Nothing -> 0 
        Just (a :% 1, b :% 1) -> fromIntegral (a * 3 + b )
        _ -> 0 

parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

type Button = (Int,Int)
type Prize = (Int,Int)

data Machine = Machine Button Button Prize  deriving (Show)

parseMachine :: Parser Machine
parseMachine = Machine <$> button <*> button <*> prize 

prize :: Parser Prize 
prize = (,) <$ string "Prize: X=" <*> decimal <* string ", Y=" <*> decimal 


button :: Parser Button 
button =  do 
    _ <- string "Button " 
    _ <- char 'A' <|> char 'B' 
    _ <- string ": X+" 
    a <- decimal 
    _ <- string ", Y+" 
    b <- decimal 
    _ <- endOfLine
    return (a,b)

machines :: ByteString -> [Machine]
machines s = map (parse' parseMachine) $ splitOnBs "\n\n" (BS.init s)

part1 :: ByteString -> IO Integer
part1 s = return (toInteger . sum . map (cost 0) $ machines s)

part2 :: ByteString -> IO Integer
part2 s = return (toInteger . sum . map (cost 10000000000000) $ machines s)

