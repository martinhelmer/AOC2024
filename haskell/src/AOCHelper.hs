{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PackageImports #-}

module AOCHelper where
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Map as M
import qualified Data.HashMap as HM
import qualified Data.Array as A

import Data.Maybe ( fromMaybe )
import           Data.List.Split
import           Data.Int

import Data.Foldable
import Data.Function (on)
import qualified Data.ByteString as BS
import qualified Data.Set as S
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Bifunctor ( Bifunctor(second) )
import Data.Hashable (Hashable)
import Data.Char (ord)

-- squareRoot = floor . sqrt . fromIntegral

squareRoot :: Integral t => t -> t
squareRoot n
   | n > 0    = babylon n
   | n == 0   = 0
   | n < 0    = error "Negative input"
   | otherwise = error "Weird"
   where
   babylon a   | a > b  = babylon b
               | otherwise   = a
      where b  = quot (a + quot n a) 2


pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [_] = undefined
pairs [x,y] = [(x,y)]
pairs (x:xs) = map (x,) xs ++ pairs xs

mapInsert:: (Ord a) => M.Map a b -> [(a,b)] -> M.Map a b
mapInsert = foldl (\r (a,b) -> M.insert a b r)

hashmapCollect:: (Ord k,Semigroup a, Hashable k) => (b -> a) -> [(k, b)] -> HM.Map k a
hashmapCollect puure = HM.fromListWith (<>) . map (second puure)

mapCollect:: (Ord k, Semigroup a) => (b -> a) -> [(k, b)] -> M.Map k a
mapCollect puure = M.fromListWith (<>) . map (second puure)


readInp :: [Char] -> IO String
readInp = readFile . (++) "../input/"

readInpT :: FilePath -> IO T.Text
readInpT = TIO.readFile . (<>) "../input/"

readInpByteSTring :: [Char] -> IO BS.ByteString
readInpByteSTring = BS.readFile  . (++) "../input/"


assertInt :: Int -> Int -> IO()
assertInt  expected result | result == expected = putStrLn $ show result ++ " - OK"
                           | otherwise  = putStrLn $ show result ++" - Fail! expected "++show expected


assertIt :: (Eq a, Show a) => a -> a -> IO ()
assertIt expected result | result == expected = putStrLn $ show result ++ " - OK"
                         | otherwise  = putStrLn $ show result ++" - Fail! expected "++show expected

printreturn :: (Show a) => a -> IO a
printreturn x = putStr (show x) >> return x

makePbmFromInts :: Int -> Int -> [Int] -> IO ()
makePbmFromInts w h im = writeFile "./message.pbm" pbm
  where pbm = "P1 " ++ show w ++ " " ++ show h ++ " " ++ unwords (map show im)

sign :: (Ord a1, Num a1, Num a2) => a1 -> a2
sign i = if i < 0 then -1 else 1

class Parseable a where
    parseString :: String -> a

instance Parseable (V.Vector Int) where
    parseString s = V.fromList (read $ "[" ++ s ++ "]")

instance Parseable (UV.Vector Int) where
    parseString s = UV.fromList (read $ "[" ++ s ++ "]")

instance Parseable [Int16] where
    parseString s  = map (\x -> read [x]::Int16) (init s)

instance Parseable [Int8] where
    parseString s  = map (\x -> read [x]::Int8) (init s)

instance Parseable [Int] where
    parseString s  = map (\x -> read [x]::Int) (init s)

parseIntoArray :: [Char] -> A.Array (Int, Int) Char
parseIntoArray s = A.listArray bounds (concat l)
    where bounds = ((0,0),(length l -1,length (head l) -1))
          l = lines s


parseIntoArrayWithBorder  :: Char -> [Char] -> A.Array (Int, Int) Char
parseIntoArrayWithBorder c s = A.listArray bounds (replicate cols c ++ concat l ++ replicate cols c )
    where bounds = ((-1,-1),(rows -1 ,cols-1))
          l = map (\l' -> c:l'++[c]) $ lines s
          rows = length l + 2
          cols = length (head l)

parseInto2dMap :: String -> M.Map (Int, Int) Char
parseInto2dMap s = M.fromList $ zip ([(x,y) | y <- [0..(rows-1)] , x <-[0..(cols-1)]]) (concat lns)
    where lns  = lines s
          rows = length lns
          cols = length . head $ lns



mapBounds :: (Ord b, Ord a) => [(a, b)] -> ((a, b), (a, b))
mapBounds a = ((x1,y1),(x2,y2))
    where x1 =  minimum . map fst $ a
          x2 =  maximum . map fst $ a
          y1 =  minimum . map snd $ a
          y2 =  maximum . map snd $ a


draw2dset :: (Foldable t) => t (Int, Int) -> String
draw2dset s = draw2dmap ( M.fromList $ map (,1::Int) (F.toList s))


draw2dmap ::  M.Map (Int, Int) Int -> String
draw2dmap m = unlines $ chunksOf (1+x2-x1) (map (\k -> intDispl $ fromMaybe 0 (M.lookup k m)) ( flip (,) <$>  [y1..y2] <*> [x1..x2]))
    where  ((x1,y1),(x2,y2)) = mapBounds (M.keys m)

draw2dcharmap :: (Enum b, Ord b) => M.Map (Int, b) Char -> [Char]
draw2dcharmap m = map (\c -> if c =='#' then '\x2588' else c) $ unlines $ chunksOf (1+x2-x1) (map (\k -> fromMaybe ' ' (M.lookup k m)) ( flip (,) <$>  [y1..y2] <*> [x1..x2]))
    where  ((x1,y1),(x2,y2)) = mapBounds (M.keys m)


draw2dchararr ::  A.Array (Int, Int) Char -> [Char]
draw2dchararr a = draw2dcharmap ( M.fromList $ map (\((y,x),v) -> ((x,y),v)) $ A.assocs a)

drawSparse :: (Int, Int) -> [(Int,Int)] -> [Char]
drawSparse (rows, cols) l = unlines . chunksOf cols . map (\c -> if S.member c s then '#' else '.') $ (,) <$> [0..(rows-1)] <*> [0..(cols-1)]
    where s= S.fromList l

intDispl ::  Int -> Char
intDispl 1 = '\x2588'
intDispl 0 = '.'
intDispl 2 = '\x2592'
intDispl 3 = '='
intDispl 4 = '\x25CD'
intDispl _ = undefined

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = maximumBy (compare `on` f)

minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minimumOn f = minimumBy (compare `on` f)


--- 
type Pos = (Int, Int)
type Dir = (Int, Int)

tp:: Pos -> Dir -> Pos
tp (a,b) (c,d) = (a+c, b+d)


splitOnBs :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
splitOnBs x y = h : if BS.null t then [] else splitOnBs x (BS.drop (BS.length x) t)
    where (h,t) = BS.breakSubstring x y

stringlisthash :: Foldable t => t [Char] -> Integer
stringlisthash l = h' (concat l)
    where h' [] = 0
          h' [x] = toInteger $ ord x - ord ' '
          h' (x:xs) = toInteger (ord x - ord ' ') * 58 + h' xs