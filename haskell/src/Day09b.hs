{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day09b (runme, runex) where

import AOCHelper (readInpByteSTring)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import qualified Data.IntPSQ as PSQ
import Data.List (partition)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Sequence as Seq
import RunUtil (RunMe, runMeByteString)
import Text.RawString.QQ

runex :: RunMe
runex =
  runMeByteString
    "Day 09 - example"
    (return example)
    part1
    (Just 1928)
    part2
    (Just 2858)

runme :: RunMe
runme =
  runMeByteString
    "-- Day 9b: Disk Frag. (Holebct)"
    (readInpByteSTring "day09.txt")
    part1
    (Just 6471961544878)
    part2
    (Just 6511178035564)

example :: ByteString
example =
  [r|2333133121414131402|]

---

data P = Num Int | Gup deriving (Show, Eq)

readinp :: String -> Int -> [P]
readinp [] _ = []
readinp "\n" _ = []
readinp [f] counter = replicate (digitToInt f) (Num counter)
readinp [f, '\n'] counter = replicate (digitToInt f) (Num counter)
readinp (f : g : xs) counter =
  replicate (digitToInt f) (Num counter)
    ++ replicate (digitToInt g) Gup
    ++ readinp xs (counter + 1)

foldit :: [(Int, P)] -> [(Int, Int)] -> [Int]
foldit ((ix, next) : xs) ft@((ix2, n2) : t)
  | ix > ix2 = []
  | otherwise = case next of
      Gup -> n2 : foldit xs t
      Num n -> n : foldit xs ft

part1 :: ByteString -> IO Integer
part1 s = do
  let q = zip [0 ..] (readinp (BS.unpack s) 0)
  let rev =
        reverse 
        . mapMaybe 
          ( \case
              (i, Num n) -> Just (i, n) 
              (_, Gup) -> Nothing
          )
          $ q
  let s' = foldit q rev
  (return . toInteger . sum) (zipWith (*) [0 ..] s')

--
type ID = Int

type Size = Int

type Pos = Int

data P2
  = File !ID !Pos !Size
  | Gap !Pos !Size

  deriving (Show, Eq, Ord)

type Bucket = PSQ.IntPSQ Int Int

type Buckets = Seq.Seq Bucket

isFile :: P2 -> Bool
isFile (File {}) = True
isFile (Gap _ _) = False

readinp2 :: String -> Int -> Int -> [P2]
readinp2 [] _ _ = []
readinp2 "\n" _ _ = []
readinp2 [f] id' pos = [File id' pos (digitToInt f)]
readinp2 [f, '\n'] id' pos = readinp2 [f] id' pos
readinp2 (f : g : xs) id' pos =
  let flen = digitToInt f
      glen = digitToInt g
   in File id' pos flen : Gap (pos + flen) glen : readinp2 xs (id' + 1) (pos + flen + glen)

mkBuckets :: [P2] -> Buckets
mkBuckets =
  foldl'
    addbucket
    (Seq.fromList (replicate 10 (PSQ.empty :: Bucket)))
  where
    addbucket :: Buckets -> P2 -> Buckets
    addbucket _ (File {}) = error "file found!"
    addbucket buckets (Gap pos size) =
      Seq.adjust' (PSQ.insert pos pos size) size buckets

--

part2 :: ByteString -> IO Integer
part2 s = do
  let (files, holes) = partition isFile $ readinp2 (BS.unpack s) 0 0
  return
    . toInteger
    . sum
    . map fileChecksum
    $ moveFiles (mkBuckets holes) (reverse files)

---

fileChecksum :: P2 -> Int
fileChecksum (File id' loc len) = id' * sum [loc .. (loc + len - 1)]
fileChecksum (Gap {}) = undefined

moveFiles :: Buckets -> [P2] -> [P2]
moveFiles _ [] = []
moveFiles holes (file : fs) =
  case movefile holes file of
    Nothing -> file : moveFiles holes fs
    Just (movedfile, updatedholes) -> movedfile : moveFiles updatedholes fs

movefile :: Buckets -> P2 -> Maybe (P2, Buckets)
movefile _ (Gap {}) = undefined
movefile holes file@(File _ loc len) = findhole >>= Just . putfile holes file
  where
    findhole =
      let q =
            mapMaybe
              (\ix -> (,ix) <$> PSQ.findMin (fromJust $ Seq.lookup ix holes))
              [len .. 9]
          f = filter (\((_, p, _), _) -> p < loc) q
       in if null f
            then Nothing
            else Just (snd . minimum $ f)

putfile :: Buckets -> P2 -> Int -> (P2, Buckets)
putfile holes (File id' _ len) ix =
  let thishole = fromJust $ holes Seq.!? ix
      (newpos, _, _, q) = fromJust $ PSQ.minView thishole
      h1 = Seq.update ix q holes
      remhole = ix - len
      remholepos = newpos + len
      addremhole = Seq.adjust' (PSQ.insert remholepos remholepos remhole) remhole h1
   in  (File id' newpos len, if remhole == 0 then h1 else addremhole)