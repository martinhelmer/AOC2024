{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module Day12 (runme, runex) where

import Text.RawString.QQ

import Data.ByteString ( ByteString, intercalate )
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict (IntMap)

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  endOfLine,
 )

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import Data.Bifunctor ( bimap )
import Data.Maybe (fromMaybe)
import Data.List ( elemIndex, inits, groupBy, group )


example :: ByteString
example =
  [r|???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
|]

runex :: RunMe
runex =
  runMeByteString
    "Day 12 - example"
    (return example)
    part1
    (Just 21)
    part2
    (Just 525152)

runme :: RunMe
runme =
  runMeByteString
    "Day 12: Hot Springs"
    (readInpByteSTring "day12.txt")
    part1
    (Just 7007)
    part2
    (Just 3476169006222)

---

-- ".??..??...?##. 1,1,3"
parseRow :: Parser (ByteString, [Int])
parseRow = (,) <$> AP.takeWhile (/= ' ') <* AP.space <*> AP.sepBy AP.decimal "," <* endOfLine

parseRow2 :: Parser (ByteString, [Int])
parseRow2 = bimap (intercalate "?" . replicate 5) (concat . replicate 5) <$> parseRow

parse' :: Parser  b -> ByteString -> b
parse' p s = either (error . show) id  $ AP.parseOnly (p <* AP.endOfInput) s
--

numCombs :: ByteString -> [Int] -> Int
numCombs "" [] = 1
numCombs "" _ = 0
numCombs bs [] = maybe 1 (const 0) (BS.elemIndex '#' bs)
numCombs bs (x:xs) = place bs x xs + skip bs (x:xs)

place :: ByteString -> Int -> [Int] -> Int
place bs 0 xs | bs == "" = if null xs then 1 else 0
              | BS.head bs == '#' = 0
              | otherwise = numCombs (BS.drop 1 bs) xs
place bs x xs | bs == "" = 0
              | BS.head bs == '.' = 0
              | otherwise = place (BS.drop 1 bs ) (x-1) xs

skip :: ByteString -> [Int] -> Int
skip bs xs | BS.head bs == '#' = 0
           | otherwise = numCombs (BS.drop 1 bs) xs

-- --
type Memo = IntMap Int
type Mkey = Int


mykey :: ByteString -> [Int] -> Int
mykey bytestring list = length list*2000 + BS.length bytestring

domemo :: Memo -> Mkey -> (Int, Memo) -> (Int, Memo)
domemo memo key v =
    case M.lookup key memo of
      Nothing -> (fst v, uncurry (M.insert key) v )
      (Just v') -> (v', memo)

numCombs' :: Memo -> ByteString -> [Int] -> (Int, Memo)
numCombs' memo "" [] = (1, memo)
numCombs' memo "" _ = (0, memo)
numCombs' memo bs [] = domemo memo (mykey bs []) (maybe 1 (const 0) (BS.elemIndex '#' bs), memo)
numCombs' memo bs l@(x:xs) = case M.lookup (mykey bs l) memo of
      (Just v) -> (v, memo)
      Nothing -> if BS.length bs < (sum l + length l -1) then (0,M.insert (mykey bs l) 0 memo) else
                (sv' + pv', M.insert (mykey bs l) (sv' + pv') memo'')

   where (sv', memo') = skip' memo bs (x:xs)
         (pv', memo'') = place' memo' bs x xs

place' :: Memo -> ByteString -> Int -> [Int] -> (Int, Memo)
place' memo bs 0 xs | bs == "" = (if null xs then 1 else 0, memo)
               | BS.head bs == '#' = (0, memo)
               | otherwise = numCombs' memo (BS.drop 1 bs) xs
place' memo bs x xs | bs == "" = (0, memo)
              | BS.head bs == '.' = (0, memo)
              | otherwise = place' memo (BS.drop 1 bs ) (x-1) xs

skip' :: Memo -> ByteString -> [Int] -> (Int, Memo)
skip' memo bs xs | BS.head bs == '#' = (0, memo)
                 | otherwise = numCombs' memo (BS.drop 1 bs) xs

numCombs2 :: (ByteString, [Int]) -> Int
numCombs2 = fst . uncurry (numCombs' M.empty)

part1 :: ByteString -> IO Integer
part1  s = do
  let q = parse' (many parseRow) s
  return . toInteger
         . sum
         . map (uncurry numCombs)
         $ q

part2 :: ByteString -> IO Integer
part2  s = do
    let q = parse' (many parseRow2) s
    return . toInteger
         . sum
         . map numCombs2
         $ q


-- other persons' solution (https://www.reddit.com/r/haskell/comments/18geaf8/advent_of_code_2023_day_12/)
getCombCount :: [Char] -> [Int] -> Int
getCombCount spr sprGrp = last $ last table
    where
        sprLen = length spr
        fstOprSpr = fromMaybe sprLen $ elemIndex '#' spr
        sprPrefixes = tail $ map reverse $ inits spr
        grpAllSprs = head . groupBy (\x y -> (x==y) || (x /= '.' && y /= '.'))

        table :: [[Int]]
        table = (replicate (fstOprSpr + 1) 1 ++ replicate (sprLen - fstOprSpr) 0) : [nextRow inp | inp <- zip sprGrp table]

        nextRow :: (Int,[Int]) -> [Int]
        nextRow (grp,prevRow) = let r' = initCells ++ [nextCell grp inp | inp <- zip3 (drop grp r') prevRow (drop grp sprPrefixes)] in r'
            where
                initCells = replicate grp 0 ++
                    if head prefix /= '.' && length (grpAllSprs prefix) == grp then
                        [head prevRow]
                    else
                        [0]
                    where
                        prefix = sprPrefixes !! max 0 (grp-1)

        nextCell :: Int -> (Int,Int,[Char]) -> Int
        nextCell cg (prevCell,prevRowCell,prefix)
            | head prefix == '.' = prevCell
            | head prefix == '?' = prevCell + if isValidPlace || isValidPlace2 then prevRowCell else 0
            | otherwise = if isValidPlace2 then prevRowCell else 0
            where
                unkownGrp = head $ group prefix
                isValidPlace = (length unkownGrp > cg) || ((length prefix < (cg+1) || prefix !! cg /= '#') && length unkownGrp == cg)
                isValidPlace2 = (length (grpAllSprs prefix) >= cg) && (length prefix < (cg+1) || prefix !! cg /= '#')