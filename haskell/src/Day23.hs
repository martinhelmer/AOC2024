{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day23 (runme, runex) where

import Text.RawString.QQ

import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, stringlisthash)
import qualified Data.HashMap as M 
import qualified "unordered-containers" Data.HashSet as S 
import Data.List (sort)
import Data.Foldable (foldl')
import Data.Containers.ListUtils (nubOrd)


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
    "--- Day 23: LAN Party ---"
    (readInpByteSTring "day23.txt")
    part1
    (Just 1330)
    part2
    (Just 117240)

---

triples :: M.Map String (S.HashSet String) -> (String, String) -> [(String, String, String)]
triples m (s1, s2) = let common = S.intersection (m M.! s1) (m M.! s2) 
            in map (\e -> (\[a, b, c] -> (a,b,c)) $ sort [s1, s2, e]) (filter (\e -> s1s2hast || (head e == 't')) $ S.toList common)
            where s1s2hast = (head s1 == 't' || head s2 == 't')

mkGraph :: [(String, String)] -> M.Map String (S.HashSet String)
mkGraph l = let ll = l ++ map (\t -> (snd t, fst t)) l
            in foldl' (\m (k,e) -> M.insertWith S.union k (S.singleton e) m)  M.empty $ ll

pair :: Parser [Char]
pair = BS.unpack <$> AP.take 2 

parsePair :: Parser (String, String)
parsePair = (,) <$> pair <* AP.char '-' <*> pair 

parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

part1 :: ByteString -> IO Integer
part1 s = do 
    let pairs = map (parse' parsePair) $ BS.lines s 
        graph = mkGraph pairs
        ttsets = nubOrd  $ concatMap (triples graph) pairs
    -- print (pairs)
    -- print (mkGraph pairs)
    -- print (ttsets)
    return . toInteger . length $ ttsets
 
setsize :: M.Map String (S.HashSet String) -> [S.HashSet String]
setsize m  = foldl' go [] (M.assocs m)
    where
        go [] (k,_) = [S.singleton k ]
        go (s:xs) (k,this) = dos : go xs (k,this) 
            where dos | S.intersection s this == s = S.insert k s 
                      | otherwise = s 


part2 :: ByteString -> IO Integer
part2 s  = do 
    let pairs = map (parse' parsePair) $ BS.lines s 
        graph = mkGraph pairs
        sets =  (setsize graph)
        largest = foldl' (\s1 s2 -> if S.size s2 > S.size s1 then s2 else s1) S.empty sets 
    -- print (sort $ S.toList largest)
    return $ stringlisthash (sort $ S.toList largest)

