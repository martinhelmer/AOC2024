{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day03 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  decimal,
  char,
  endOfInput,
  endOfLine,
  isDigit,
  many1,
  parseOnly,
  skipSpace,
  skipWhile,
  string,
  manyTill,
  anyChar,
  sepBy,
  choice,
 )

import Data.Attoparsec.Combinator (lookAhead)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as AP

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, Pos, Dir, tp)
import qualified BSArray as BSA
import qualified Data.List.Split as BS

parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

example :: ByteString
example =
  [r|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|]

parseMul1' :: Parser Int
parseMul1'  = (*) <$ string "mul("  <*> decimal <* char ','  <*> decimal <* char ')'

runex :: RunMe
runex =
  runMeByteString
    "Day 03 - example"
    (return example)
    part1
    (Just 161)
    part2
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 3: Mull It Over ---"
    (readInpByteSTring "day03.txt")
    part1
    (Just 166357705)
    part2
    (Just 88811886)

---
p1 :: Parser [Int]
p1 = ((many ((manyTill anyChar (lookAhead parseMul1')) *> parseMul1')) <* (many anyChar))

part1 :: ByteString -> IO Integer
part1 s = return . toInteger . sum $ (parse' p1 s)

filter1 :: ByteString -> ByteString
filter1 s  = BS.pack $ concat $
          (parse'
            (manyTill
              ((manyTill anyChar ((void (string "don't()")) <|> endOfInput) <* (manyTill anyChar ((void (string "do()")) <|> endOfInput)))
              ) endOfInput)) s

tokenise :: ByteString -> ByteString  -> [ByteString]
tokenise x y = h : if BS.null t then [] else tokenise x (BS.drop (BS.length x) t)
    where (h,t) = BS.breakSubstring x y

filter2 :: ByteString -> ByteString
filter2 =  BS.concat . map (fst . BS.breakSubstring "don't()") . tokenise "do()"

part2 :: ByteString -> IO Integer
part2 s = return . toInteger . sum $ (parse' p1 (filter2 s))