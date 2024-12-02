{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day02 (runme) where

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import Data.Maybe (mapMaybe )
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (
    Parser,
    decimal,
    space,
    string,
    takeTill, parseOnly,
 )
import Data.Char (isDigit)
import Data.List (foldl')
-- import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)


--  Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
data Color = Red | Green | Blue deriving (Enum, Show)
data ColorVal = ColorVal Color Int deriving (Show)

parseColor :: Parser Color
parseColor = (string "blue" >> return Blue) <|> (string "red" >> return Red) <|> (string "green" >> return Green)

parseCV :: Parser ColorVal
parseCV = flip ColorVal <$ takeTill isDigit <*> decimal <* space <*> parseColor

-- parseCV  = do
--     _ <- takeTill isDigit
--     v <- decimal
--     skipSpace
--     c <- parseColor
--     return $ ColorVal c v

parseGame :: Parser [ColorVal]
parseGame = do
    _ <- takeTill (':' ==)
    many parseCV

maxes :: [ColorVal] -> (Int, Int, Int)
maxes = foldl' go (0, 0, 0)
  where
    go (r, g, b) cv = case cv of
        ColorVal Red v -> (max r v, g, b)
        ColorVal Green v -> (r, max g v, b)
        ColorVal Blue v -> (r, g, max b v)


getmaxes :: ByteString -> [(Int, Int, Int)]
getmaxes  = map (maxes . either undefined id . parseOnly parseGame ) . B.lines 


possible :: (Ord a1, Ord a2, Ord a3, Num a1, Num a2, Num a3) => (a4, (a1, a2, a3)) -> Maybe a4
possible (i, (r,g,b)) | r > 12 || g > 13 || b > 14 = Nothing
                   | otherwise = Just i 

--------
runme :: RunMe
runme = runMeByteString "Day 2: Cube Conundrum"
              (readInpByteSTring "day02.txt") 
              (fmap toInteger . part1)
              (Just 2512)
              (fmap toInteger . part2)
              (Just 67335)

part1 :: ByteString -> IO Int
part1 s = do
    return . sum . mapMaybe possible $ zip [1..] $ getmaxes s 

part2 :: ByteString -> IO Int
part2 s = do
    return . sum . map (\(r,g,b) -> r * g * b) $ getmaxes s 
