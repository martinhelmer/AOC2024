{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BangPatterns #-}

module Day08 (runme, runex) where

import AOCHelper (readInpByteSTring)
import RunUtil (RunMe, runMeByteString)


import Text.RawString.QQ ( r )

import Control.Applicative ( Alternative((<|>), many) )
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  endOfInput,
  endOfLine,
  parseOnly,
 )
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (scanl', foldl1')
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Either (fromRight)

runex :: RunMe
runex =
  runMeByteString
    "Day 8 - example"
    (return example)
    part1
    (Just 6)
    part2
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "Day 8: Haunted Wasteland "
    (readInpByteSTring "day08.txt")
    part1
    (Just 23147)
    part2
    (Just 22289513667691)

example :: ByteString
example =
  [r|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
|]

--- Data
data Instruction = Lft | Rght deriving (Show, Eq)

lorr :: Char -> Instruction
lorr 'L' = Lft
lorr 'R' = Rght
lorr _   = error "unsupported instruction"

type Node = (ByteString, (ByteString, ByteString))
type Graph = HashMap ByteString (ByteString, ByteString)
--- Parse

parseInput :: Parser ([Instruction], Graph)
parseInput = (,) <$> parseInstr <* endOfLine <*> (HM.fromList <$> many parsePreNode)

parseInstr :: Parser [Instruction]
parseInstr = many (lorr <$> (AP.char 'L' <|> AP.char 'R')) <* endOfLine

-- AAA = (BBB, BBB)
parsePreNode :: Parser Node
parsePreNode =
  (\a b c -> (a, (b, c)))
    <$> AP.take 3
    <* " = ("
    <*> AP.take 3
    <* ", "
    <*> AP.take 3
    <* AP.char ')'
    <* endOfLine

parse :: Parser b -> ByteString -> b
parse parser s = fromRight undefined $ parseOnly (parser <* endOfInput) s
---- 
stepstoZ :: Graph -> [Instruction] -> ByteString -> Integer
stepstoZ graph instructions sn = toInteger
         . length
         . takeWhile ((/=) 'Z' . B.last )
         . scanl' (\n i -> pick i (graph HM.! n)) sn
         $ cycle instructions
         where pick Lft = fst
               pick _   = snd 

part1 :: ByteString -> IO Integer
part1 s = do
  let (!instructions, !graph) = parse parseInput s
  return . stepstoZ graph instructions
         $ "AAA"

part2 :: ByteString -> IO Integer
part2 s = do
  let (!instructions, !graph) = parse parseInput s
  return . foldl1' lcm
         . map (stepstoZ graph instructions)
         . filter ((==) 'A' . B.last)
         $ HM.keys graph
