{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day23b (runme, runex) where

import AOCHelper (readInpByteSTring, stringlisthash)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (chr, ord)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (foldl')
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List (find, sort)
import Data.Maybe (listToMaybe, mapMaybe)
import RunUtil (RunMe, runMeByteString)
import Text.RawString.QQ

example :: ByteString
example =
  [r|kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
|]

runex :: RunMe
runex =
  runMeByteString
    "Day 23 - example"
    (return example)
    part1
    (Nothing)
    part2
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 23b: LAN Party (IntMap)"
    (readInpByteSTring "day23.txt")
    part1
    (Just 1330)
    part2
    (Just 117240)

---
intKey :: String -> Int
intKey [] = 0
intKey ['t', t] = 1000 + (ord t - ord 'a')
intKey (x : xs) = 1 + ord x - ord 'a' + 28 * intKey xs

keyInt :: Int -> [Char]
keyInt n
  | n >= 1000 = ['t', chr (n - 1000 + ord 'a')]
  | otherwise =
      let (h, l) = n `divMod` 28
       in [chr (l - 1 + ord 'a'), chr (h - 1 + ord 'a')]

triples :: IM.IntMap (IS.IntSet) -> (Int, Int) -> [(Int, Int, Int)]
triples m (s1, s2) =
  let common = IS.intersection (m IM.! s1) (m IM.! s2)
   in map
        (\e -> (\[a, b, c] -> (a, b, c)) $ sort [s1, s2, e])
        (filter (>= 1000) $ IS.toList common)

mkGraph :: [(Int, Int)] -> IM.IntMap (IS.IntSet)
mkGraph l =
  let ll = l ++ map (\t -> (snd t, fst t)) l
   in foldl' (\m (k, e) -> IM.insertWith IS.union k (IS.singleton e) m) IM.empty $ ll

pair :: Parser Int
pair = intKey . BS.unpack <$> AP.take 2

parsePair :: Parser (Int, Int)
parsePair = (,) <$> pair <* AP.char '-' <*> pair

parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

part1 :: ByteString -> IO Integer
part1 s = do
  let pairs = map (parse' parsePair) $ BS.lines s
      graph = mkGraph pairs
      ttsets = nubOrd $ concatMap (triples graph) pairs
  return . toInteger . length $ ttsets

-- pick n elements from s. keep the order
get :: Int -> [a] -> [[a]]
get n s
  | n == 0 = [[]]
  | length s == n = [s]
  | otherwise = get n (tail s) ++ map (head s :) (get (n - 1) (tail s))

findNall :: IM.IntMap IS.IntSet -> Int -> Maybe IS.IntSet
findNall graph n =
  listToMaybe
    . mapMaybe (\k -> IS.insert k <$> findN k)
    $ IM.keys graph
  where
    findN :: Int -> Maybe IS.IntSet
    findN node = find fits subsets
      where
        neighbors = graph IM.! node
        subsets = map IS.fromList $ get n (IS.toList neighbors)
        fits s =
          all
            ( \m ->
                IS.difference
                  (IS.insert node s)
                  (graph IM.! m)
                  == IS.singleton m
            )
            (IS.toList s)

part2 :: ByteString -> IO Integer
part2 s = do
  let pairs = map (parse' parsePair) $ BS.lines s
      graph = mkGraph pairs
      biggestSet = head $ mapMaybe (findNall graph) (reverse [1 .. 13])
  return
    . stringlisthash
    . sort
    . map keyInt
    . IS.toList
    $ biggestSet
