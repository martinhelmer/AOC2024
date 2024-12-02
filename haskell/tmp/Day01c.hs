{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Day01c (runme) where

import AOCHelper
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Char (ord)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, mapMaybe)

import RunUtil (RunMe, runMeByteString)
runme :: RunMe
runme = runMeByteString 
        "Day 1: (BS) Trebuchet?!" 
        (readInpByteSTring "day01.txt") 
        (fmap toInteger . part1) 
        (Just 55386) 
        (fmap toInteger . part2) 
        (Just 54824)

---------

dw :: V.Vector ByteString
dw = let !q =  V.fromList ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] in q 

dwr :: V.Vector ByteString
dwr = let !q = V.fromList . map B.reverse $ ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] in q 

fromWords ::  V.Vector ByteString -> Int -> ByteString -> Maybe Int
fromWords _ 10 _ = Nothing
fromWords v n s
    | (v V.! (n-1)) `B.isPrefixOf` s = Just n
    | otherwise = fromWords v (n + 1) s

fdd :: V.Vector ByteString -> Bool -> ByteString -> Maybe Int
fdd v check_words t =
    let
        x = ord (B.head t) - ord '0'
        go
            | 0 <= x && x <= 9 = Just x
            | check_words = fromWords v 1 t
            | otherwise = Nothing
     in
        go

getfirst :: (ByteString -> Maybe Int) -> [ByteString] -> Int
getfirst f (x : xs) = fromMaybe (getfirst f xs) (f x)

fd :: Bool -> ByteString -> Int
fd cw s = (head . mapMaybe (fdd dw cw) $ B.tails s) * 10 + (head . mapMaybe (fdd dwr cw) $ B.tails (B.reverse s))

part1 :: ByteString -> IO Int
part1 s = do
    return $ sum $ map (fd False) $ B.lines s

part2 :: ByteString -> IO Int
part2 s = do
    return $ sum $ map (fd True) $ B.lines s
