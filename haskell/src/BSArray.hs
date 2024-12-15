{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module BSArray (BSArray
               , Index
               , updateLoc
               , rows
               , cols
               , row
               , indices
               , BSArray.lookup
               , makeBSarray
               , lookupMaybe
               , BSArray.elemIndex
               , elemIndices
               , intIndex
               , stringColsFromLeft
               , bsColsFromLeft
               , unpackRows
               , rawIndex2Index
               , rawIndex
               , inRange 
               , toMap
               ) where
import Data.ByteString (ByteString)
import Data.Hashable
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8(append, singleton, take, drop)
import Prelude hiding (take, drop)
import qualified Data.Map as M 

type Index = (Int, Int)

newtype Row = Row Int deriving (Eq, Show)
newtype Col = Col Int deriving (Eq, Show)

data BSArray = BSArray
    { contents :: ByteString
    , _rows :: Row
    , _cols :: Col
    } deriving (Eq)

instance  Show BSArray where
    show bsa = B.unpack $  contents bsa

instance  Hashable BSArray where
    hashWithSalt i (BSArray bs _ _ ) = hashWithSalt i bs


rows :: BSArray -> Int
rows (BSArray _ (Row rows') (Col _)) = rows'

cols :: BSArray -> Int
cols (BSArray _ (Row _) (Col cols')) = cols'


makeBSarray :: ByteString -> BSArray
makeBSarray s = BSArray s (Row rows') (Col cols')
    where (cols', rows') = case (B.elemIndex '\n' s , B.last s == '\n') of
            (Nothing, _) -> (B.length s, 1)
            (Just l ,True)  -> (l, B.length s `div` (l+1))
            (Just l, False) -> (l, (B.length s+1) `div` (l+1))

updateLoc :: BSArray -> Index -> Char -> BSArray
updateLoc bsa ix c = makeBSarray $ take (ix') bs `append ` singleton c `append` drop (ix'+1) bs
        where ix' = rawIndex bsa ix
              bs = contents bsa

unsafeLookup :: BSArray -> Index ->  Char
unsafeLookup (BSArray s _ (Col cols')) (ir, ic) = B.index s (ic + ir * (cols' +1))

lookup :: BSArray -> Index ->  Char
lookup (BSArray s _ (Col cols')) (ir, ic) | ir < 0 = error "Negative row index!"
                                          | ic < 0 = error "Negative column index!"
                                          | otherwise = B.index s (ic + ir * (cols' +1))

inRange :: BSArray -> Index -> Bool 
inRange bsa (ir,ic) = not (ir < 0 || ir >= rows bsa || ic < 0 || ic >= cols bsa)

lookupMaybe :: BSArray -> Index -> Maybe Char
lookupMaybe a@(BSArray s (Row rows') (Col cols')) (ir, ic)
    | not (inRange a (ir,ic))= Nothing
    | otherwise = Just (B.index s (ic + ir * (cols' +1)))

rawIndex2Index :: BSArray -> Int -> Index
rawIndex2Index bs ix = (ix `div` (cols bs + 1 ), ix `rem` (cols bs + 1 ))


-- refactor (switch arguments to conform to haskell standard)
elemIndex :: BSArray -> Char -> Maybe Index
elemIndex bs c =rawIndex2Index bs <$> B.elemIndex c (contents bs)

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> BSArray -> [Index]
elemIndices  c bs = map (rawIndex2Index bs) $ B.elemIndices c (contents bs)

intIndex :: BSArray -> Index -> Int
intIndex bs (row', col) = (cols bs * row') + col

rawIndex :: BSArray -> Index -> Int
rawIndex bs (row', col) = ((cols bs +1 ) * row') + col

length' :: BSArray -> Int
length' bs = B.length (contents bs )

indices :: BSArray -> [Index]
indices bs = [(r,c) | r <- [0.. (rows bs -1)]  , c <- [0..(cols bs -1)]]

toMap :: BSArray -> M.Map Index Char 
toMap bs = M.fromList . map (\ix -> (ix, unsafeLookup bs ix)) . indices $ bs 

row :: BSArray -> Int -> ByteString
row bs rownum = let cols' = cols bs in
        B.take cols' . B.drop (rownum*(cols' +1)) $ contents bs

stringColsFromRight :: BSArray -> [String]
stringColsFromRight bs = map (\col' ->  map (\row' -> BSArray.lookup bs (row', col')) [0.. (rows bs -1)]  ) (reverse [0..(cols bs -1)])

stringColsFromLeft :: BSArray -> [String]
stringColsFromLeft bs = map (\col' ->  map (\row' -> BSArray.lookup bs (row', col')) [0.. (rows bs -1)]  ) [0..(cols bs -1)]

bsColsFromLeft :: BSArray -> [ByteString]
bsColsFromLeft bs = map (\col' -> B.pack $ map (\row' -> BSArray.lookup bs (row', col')) [0.. (rows bs -1)]  ) [0..(cols bs -1)]


unpackRows :: BSArray -> [ByteString]
unpackRows = B.lines . contents