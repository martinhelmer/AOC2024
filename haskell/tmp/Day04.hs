{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day04 (runme) where

import AOCHelper (readInpByteSTring)
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (
    Parser,
    char,
    decimal,
    parseOnly,
    space,
    takeTill,
 )
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as S
import RunUtil (RunMe, runMeByteString)

--------
runme :: RunMe
runme =
    runMeByteString
        "Day 4: Scratchcards"
        (readInpByteSTring "day04.txt")
        part1
        (Just 32001)
        part2
        (Just 5037841)

-------

--  Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
parseCard :: Parser Int
parseCard = do
    _ <- takeTill (':' ==) *> char ':'
    winners <- S.fromList <$> (many (many space *> decimal)::Parser [Int]) 
    _ <- space *> char '|'
    haves <- many (many space *> decimal)
    return (length . filter (`S.member`  winners) $ haves)

points :: Int -> Integer
points 0 = 0
points n = 1 `shiftL` (n - 1)

cards :: ByteString -> [Int]
cards = map ((\(Right c) -> c) . parseOnly parseCard) . B.lines

play :: [Int] -> [Integer] -> Integer
play [] [] = 0
play (win:wins) (have:haves) = have + play wins newhaves
    where newhaves = map (+ have) (take win haves) <> drop win haves

part1 :: ByteString -> IO Integer
part1 = return . sum . map points . cards

part2 :: ByteString -> IO Integer
part2 s = do
    let wins = cards s
    let haves = replicate (length wins) 1::[Integer]
    return $ play wins haves
