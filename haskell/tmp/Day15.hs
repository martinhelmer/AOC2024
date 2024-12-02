{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use camelCase" #-}

module Day15 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  decimal,
  endOfInput,
  endOfLine,
  isDigit,
  many1,
  parseOnly,
  skipSpace,
  skipWhile,
 )
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (foldl', sort )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashPSQ as  PSQ
import Data.HashPSQ (HashPSQ)
import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import Data.Char (ord)
import qualified Data.IntMap.Strict as IM
import Data.Maybe ( fromMaybe )

example :: ByteString
example =
  [r|rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7|]

runex :: RunMe
runex =
  runMeByteString
    "Day x - example"
    (return example)
    part1
    (Just 1320)
    part2
    (Just 145)

runme :: RunMe
runme =
  runMeByteString
    "Day 15: Lens Library"
    (readInpByteSTring "day15.txt")
    part1
    (Just 504036)
    part2
    (Just 295719)

---
type MYPSQ = HashPSQ ByteString Int Int

parse' :: Parser  b -> ByteString -> b
parse' p s =  let !q = either (error . show) id  $ AP.parseOnly (p <* endOfInput) s in q

parseLens :: Parser Lens
parseLens = Lens <$> AP.takeWhile(AP.isAlpha_ascii) <*> AP.anyChar <*> (decimal <|> pure 0)

data Lens = Lens {lensLabel :: ByteString
                  , lensInstr :: Char
                  , lensFocal :: Int
                  }  deriving (Show)

hash :: ByteString -> Int
hash = BS.foldl' (\acc c -> (acc + ord c) * 17 `rem` 256 ) 0

splitinput :: ByteString -> [ByteString]
splitinput = BS.splitWith (== ',') . head . BS.lines

part1 :: ByteString -> IO Integer
part1  = return . toInteger . sum . map hash . splitinput

part2 :: ByteString -> IO Integer
part2 s = do
  let lenses = map  (parse' parseLens) . splitinput $ s
  let !boxedLenses = snd $ foldl'  upd   (0, PSQ.empty) lenses
  return . toInteger 
         . fst
         . foldl' 
            (\(sum', counter) (label, focal) ->
              let !h = hash label
              in (sum' + (countGet counter h +1) * (h+1) * focal, counterIncOne counter h)) 
            (0, IM.empty) 
        . extract 
        $ boxedLenses

upd :: (Int ,MYPSQ) -> Lens -> (Int, MYPSQ)
upd (lastPrio , psq) (Lens label instr focal) = case instr of
      '-' -> (lastPrio, PSQ.delete label psq)
      '=' -> PSQ.alter alterf label psq
      _ -> undefined
      where alterf Nothing = (lastPrio + 1 , Just (lastPrio+1, focal))
            alterf (Just (p,_)) = (lastPrio, Just (p, focal))

extract :: MYPSQ -> [(ByteString, Int)]
extract psq = case PSQ.findMin psq of
    Nothing -> []
    Just (label, _, focal) -> (label, focal):(extract (PSQ.deleteMin psq))


type Counter = IM.IntMap Int

counterIncOne :: Counter -> Int -> Counter
counterIncOne c i = IM.insertWith (+) i 1 c

countGet :: Num a => IM.IntMap a -> IM.Key -> a
countGet c k = fromMaybe 0 (IM.lookup k c)