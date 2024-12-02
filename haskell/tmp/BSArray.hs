{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module BSArray (BSArray
               , rows
               , cols
               , row
               , BSArray.lookup
               , makeBSarray
               , lookupMaybe
               , BSArray.elemIndex
               , BSArray.elemIndices
               , intIndex
               , stringColsFromLeft
               , bsColsFromLeft
               , unpackRows
               , rawIndex2Index
               , rawIndex
               ) where
import Data.ByteString (ByteString)
import Data.Hashable
import PosDir 
import qualified Data.ByteString.Char8 as B

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

lookup :: BSArray -> Index ->  Char
lookup (BSArray s _ (Col cols')) (ir, ic) | ir < 0 = error "Negative row index!"
                                          | ic < 0 = error "Negative column index!"
                                          | otherwise = B.index s (ic + ir * (cols' +1))

lookupMaybe :: BSArray -> Index -> Maybe Char
lookupMaybe a@(BSArray s (Row rows') (Col cols')) (ir, ic)
    | ir < 0 || ir >= rows' || ic < 0 || ic >= cols' = Nothing
    | otherwise = Just (B.index s (ic + ir * (cols' +1)))

rawIndex2Index :: BSArray -> Int -> (Int, Int)
rawIndex2Index bs ix = (ix `div` (cols bs + 1 ), ix `rem` (cols bs + 1 ))


-- refactor (switch arguments to conform to haskell standard)
elemIndex :: BSArray -> Char -> Maybe Index
elemIndex bs c = case i of
        Nothing -> Nothing
        Just ix -> Just (rawIndex2Index bs ix)
    where i = B.elemIndex c (contents bs)

elemIndices :: Char -> BSArray -> [Index]
elemIndices  c bs = map (rawIndex2Index bs) $ B.elemIndices c (contents bs)

intIndex :: BSArray -> Index -> Int
intIndex bs (row', col) = (cols bs * row') + col

rawIndex :: BSArray -> Index -> Int
rawIndex bs (row', col) = ((cols bs +1 ) * row') + col


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