{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BangPatterns #-}

module Day01 (run, runme) where

import AOCHelper
import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Data.Maybe ( fromMaybe )

import RunUtil (runMeString, RunMe)
runme :: RunMe
runme = runMeString "--- Day 1: Trebuchet?! ---" 
              (readInp "day01.txt") 
              (fmap toInteger . part1)
              (Just 55386)
              (fmap toInteger . part2)
              (Just 54824)
----------------------------------    

digwords :: [String]
digwords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

fromDigiwords :: [String] -> Int -> String -> Maybe Int
fromDigiwords [] _ _ = Nothing
fromDigiwords (x : xs) n s
        | x `isPrefixOf` s = Just n
        | otherwise = fromDigiwords xs (n + 1) s

finddigitforwards' :: [String] -> Bool -> String -> Int
finddigitforwards' wl cw s@(x : xs)
    | isDigit x = digitToInt x
    | otherwise = if cw then fromMaybe (finddigitforwards' wl cw xs) (fromDigiwords wl 1 s) else finddigitforwards' wl cw xs

finddigitbackwards :: Bool -> String -> Int
finddigitbackwards cw s = finddigitforwards' (map reverse digwords) cw (reverse s)

finddigitforwards :: Bool -> String -> Int
finddigitforwards = finddigitforwards' digwords

fd :: Bool -> String -> Int
fd cw s = finddigitforwards cw s * 10 + finddigitbackwards cw s


run :: IO ()
run = do
    putStrLn "--- Day 1: Trebuchet?! ---"
    putStr " Part1: "
    readInp "day01.txt" >>= part1 >>= assertInt 55386
    putStr " Part2: "
    readInp "day01.txt" >>= part2 >>= assertInt 54824

part1 :: String -> IO Int
part1 s = do
    return $ sum $ map (fd False) $ lines s

part2 :: String -> IO Int
part2 s = do
    return $ sum $ map (fd True) $ lines s

