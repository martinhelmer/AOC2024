{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# LANGUAGE BangPatterns #-}


module Day03 (runme, runex) where

import Text.RawString.QQ

import AOCHelper (readInpByteSTring, Pos, tp)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit, digitToInt)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import RunUtil (RunMe, runMeByteString)
import Data.Bits (shiftL, (.&.), (.|.))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

example :: ByteString
example = [r|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|]

runex :: RunMe
runex =
    runMeByteString
        "Day 3 - example"
        (return example)
        part1
        (Just 4361)
        part2
        (Just 467835)

ndig :: Int -> Int
ndig 0 = 0
ndig n = 1 + ndig (n `div` 10)


tconc :: (Semigroup a, Semigroup b) => (a, b) -> (a, b) -> (a, b)
tconc (a,b) (a', b') = ( a <> a', b <> b')

dayParseRow :: (Char -> Bool) -> Int -> ByteString -> ([(Pos, Integer)], [Pos])
dayParseRow p row = xtract . B.foldl' go (0, 0, [], [])
  where
    go (col, temp, numlist, astlist) c
        | isDigit c = (col, temp * 10 + digitToInt c, numlist, astlist)
        | not (isDigit c) && temp == 0 = (col + 1, temp, numlist, doastlist 1)
        | otherwise = (col + 1 + ndig temp, 0, ff col temp numlist, doastlist (1 + ndig temp))
      where
        doastlist n = if p c then (row, col+n) : astlist else astlist

    xtract (col, temp, c, d) = if temp > 0  then (ff col temp c, d) else (c,d)
    ff col v c = case ndig v of
                1 -> ((row, col + 1), toInteger v):c
                n -> ((row, col + n), toInteger v):((row, col + 1), toInteger v):c


--------
runme :: RunMe
runme =
    runMeByteString
        "Day 3: Gear Ratios "
        (readInpByteSTring "day03.txt")
        part1
        (Just 544664)
        part2
        (Just 84495585)

mkword :: ByteString -> Integer
mkword s = let w = B.foldl' (\acc c -> acc `shiftL` 1 + if (c /= '.') && not (isDigit c) then 1 else 0) 0 s
        in w .|. w * 2 .|. w `div` 2

wordz :: (ByteString, Integer) -> [Integer]
wordz (s, i) =  xtract q
    where q = foldl' go (0::Integer, False, []) $ zip [1..] (B.unpack s)
          go acc@(temp, touch, xs) (ix,x)
            | temp == 0 && not (isDigit x) = acc
            | isDigit x = ( temp * 10 + digitToInteger x, touch || touches ix, xs)
            | otherwise = (0, False, if touch then temp:xs else xs)
          xtract (temp, touch, l) = if temp > 0 && touch then  temp:l else l
          touches ix = 1 `shiftL` (B.length s - ix) .&. i > 0
          digitToInteger = toInteger . digitToInt

part1 :: ByteString -> IO Integer
part1 s = do
    let l = map mkword $ B.lines s
    let rr =  map (\(a,b,c) -> a .|. b .|. c ) $ zip3 l (0:l) (tail l <> [0])
    let rownums = zipWith (curry wordz) (B.lines s) rr
    -- putStrLn $ intercalate "\n" ( map show rownums)
    return $ sum . concat $  rownums

neighbors :: Pos -> [Pos]
neighbors p = map (tp p) [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]

ss :: M.Map Pos Integer -> Pos -> S.Set Integer
ss nummap p = S.fromList (mapMaybe (`M.lookup` nummap) (neighbors p) )

ss' :: ((Int,Int) -> Int) -> IM.IntMap Int -> Pos -> IS.IntSet
ss' p2i nummap p = IS.fromList (mapMaybe ((`IM.lookup` nummap) . p2i) (neighbors p) )


part2 :: ByteString -> IO Integer
part2 s = do
    let p2intf = p2int (B.length . head $ B.lines s)
    let (_, (nums, asts)) = foldl' (\(ix, acc) l -> (ix+1, tconc acc (dayParseRow (== '*') ix l))) (0, ([], []))$ B.lines s
    let astmap = S.fromList asts
    -- let nummap = M.fromList nums
    let nummap' = IM.fromList $ map (\(p,i) -> (p2intf p,fromInteger i)) nums
    let q = filter ((==) 2 . IS.size ) $ map (ss' p2intf nummap') (S.toList astmap)
    --let q = filter ((==) 2 . length ) $ map ((ss) nummap) (S.toList astmap)

    return $ sum . map (\s' -> product  (map toInteger $ IS.toList s')) $ q

p2int :: Int -> (Int, Int) -> Int
p2int width (row, col) = row * width + col

int2p :: Integral b => b -> b -> (b, b)
int2p width i = (i `div` width, i `rem` width)