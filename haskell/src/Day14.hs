{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BangPatterns #-}

module Day14 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  decimal,
  signed,
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
import AOCHelper (readInpByteSTring, drawSparse, draw2dset)
import PosDir (Pos, Loc, Dir)
import qualified BSArray as BSA
import Data.Function (on)
import Data.List (minimumBy)
import Data.Ratio (denominator)
import Data.Maybe (mapMaybe)

runex :: RunMe
runex =
  runMeByteString
    "Day 14 - example"
    (return example)
    part1ex
    (Just 12)
    part2ex
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 14: Restroom Redoubt ---"
    (readInpByteSTring "day14.txt")
    part1
    (Just 218619120)
    part2
    (Just 7055)

---
example :: ByteString
example =
  [r|p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
|]

data Robot = Robot !(Int,Int) !(Int,Int) deriving (Show)
xval (Robot (x,_) _) = x 
yval (Robot (_,y) _) = y 

parseT ::ByteString -> Parser (Int,Int)
parseT skip = (,) <$ string skip <*> signed decimal <* char ',' <*> signed decimal

parseRobot ::  Parser Robot
parseRobot =  Robot  <$> parseT "p=" <*> parseT " v="

parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

move :: (Int, Int) -> Int -> Robot -> Robot
move (xsize, ysize) n (Robot (x,y) (vx, vy)) = Robot ((x + n * vx) `mod` xsize, (y + n * vy) `mod` ysize) (vx, vy)

move1 :: (Int, Int) -> Robot -> Robot
move1 (xsize, ysize) (Robot (x,y) (vx, vy)) = Robot ((x + vx) `mod` xsize, (y + vy) `mod` ysize) (vx, vy)

quadrant :: (Int, Int) -> Robot -> Maybe Int
quadrant (mx, my) (Robot (x,y) _ )
    | x == middlex || y == middley = Nothing
    | x < middlex = if y < middley then Just 1 else Just 2
    | otherwise = if y < middley then Just 3 else Just 4
  where middlex = mx `quot` 2
        middley = my `quot` 2

quadcount :: (Num a, Num b, Num c, Num d) => (Int, Int) -> (a, b, c, d) -> [Robot] -> (a, b, c, d)
quadcount _ t [] = t
quadcount s (a,b,c,d) (rob:robots) = quadcount s q' robots
  where q' = case quadrant s rob of
              Nothing -> (a,b,c,d)
              Just 1 -> (a+1,b,c,d)
              Just 2 -> (a,b+1,c,d)
              Just 3 -> (a,b,c+1,d)
              Just 4 -> (a,b,c,d+1)
              _ -> undefined

go1 :: Monad m => (Int, Int) -> ByteString -> m Integer
go1 spacesize s = do
    let t = (0::Integer,0::Integer,0::Integer,0::Integer)
    let movedrobots = map (move spacesize 100 . parse' parseRobot ) $ BS.lines s
    -- print (movedrobots)
    return  . (\(a,b,c,d) -> a*b*c*d) . quadcount spacesize t $ movedrobots


part1ex :: ByteString -> IO Integer
part1ex = go1 (11,7)

part1 :: ByteString -> IO Integer
part1  = go1 (101, 103)

part2ex :: ByteString -> IO Integer
part2ex = go2 (11,7)

part2 :: ByteString -> IO Integer
part2 =  go2 (101, 103)


onesec :: (Int, Int) -> [Robot] -> [Robot]
onesec z = map (move1 z)

var :: (Int, Int) -> [Robot] -> Int
var (mx, my) = sum . map dz
    where dz (Robot (x,y) _)= let dx = x-mx `quot` 2; dy = (y-my `quot` 2 ) in dx*dx + dy*dy

findvarminimums :: [[Robot]] -> (Int, Int)
findvarminimums inf = (xmin, ymin)
      where !vars =  zip (map (\r -> (variance . map xval $ r, variance . map yval $ r)) inf) [0..103]
            xmin = snd $ minimumBy (compare `on` (fst . fst)) $ take 101 vars
            ymin = snd $ minimumBy (compare `on` (snd . fst)) $ take 103 vars

isIntSol :: (Int, Int) -> (Int, Int) -> Int -> Maybe Int
isIntSol (firstx, firsty) (xmod, ymod) x =
      case denominator (f x) of 
          1 -> Just (x * xmod + firstx) 
          _ -> Nothing 
    where f :: Int -> Rational 
          f x' = ( toRational (xmod * x' + firstx - firsty) ) / (toRational ymod)
    

go2 :: (Int, Int) -> ByteString  -> IO Integer
go2 z s = do
  let robots =  map (parse' parseRobot ) $ BS.lines s
  let infinity = iterate (onesec z) robots
  -- let answer = length .  takeWhile (> 410000) $ map (var z) infinity
  ---putStrLn (draw2dset (map (\(Robot t _) -> t) (infinity !! (answer))))
  -- x 101 
  -- y 103 
  -- putStrLn (draw2dset (map (\(Robot t _) -> t) (infinity !! (answer))))
  return . toInteger . head . (mapMaybe (isIntSol (findvarminimums infinity) z )) $ [0..]

variance :: [Int]  -> Float
variance l = (sum . map (\v -> (v - average)^(2::Int)) $ fl ) / len
    where fl = map fromIntegral l :: [Float]
          len = fromIntegral (length l)
          average = (fromIntegral . sum $ l) / len 

