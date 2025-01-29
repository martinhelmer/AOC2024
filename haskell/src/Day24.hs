{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day24 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as AP
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

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, splitOnBs)
import qualified BSArray as BSA
import Data.Maybe (fromJust, mapMaybe)
import Data.Attoparsec.Text (letter)
import qualified Data.Map as M
import Data.Bits ((.|.), (.&.), xor)
import Data.List (sort)
import Data.List (intercalate)
import Data.Char (ord)


runex :: RunMe
runex =
  runMeByteString
    "Day 24 - example"
    (return example)
    part1
    (Just 4)
    part2
    (Nothing)


runme :: RunMe
runme =
  runMeByteString
    "--- Day 24: Crossed Wires (TODO)"
    (readInpByteSTring "day24.txt")
    part1
    (Just 56729630917616)
    part2
    (Just 1936591348205959772)

--
example :: ByteString
example =
  [r|x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
|]

---
data Op = AND | XOR | OR deriving (Eq, Show, Enum, Ord, Read)
data Wire = SimpleWire String Int | ExprWire String Op String String deriving (Eq, Show)

setId (SimpleWire i q) i' = SimpleWire i' q 
setId (ExprWire i o s1 s2) i' = ExprWire i' o s1 s2  

wId :: Wire -> String
wId (SimpleWire s _) = s 
wId (ExprWire s _ _ _) = s 

wArgs (ExprWire _ _ a1 a2) = (a1, a2)

readOp ::String -> Op
readOp = read

parseSimple :: Parser (String, Wire)
parseSimple = (\s i  -> (s, SimpleWire s i))<$> 
              ((BS.unpack) <$> AP.take 3) <* AP.string ": "
              <*> ( AP.decimal)

parseExpr :: Parser (String, Wire)
parseExpr = do
    w1 <- ((BS.unpack) <$> AP.take 3)
    _ <- AP.space
    o <- (AP.string "AND" <|> AP.string "XOR" <|> AP.string "OR")
    _ <- AP.space
    w2 <-  ((BS.unpack) <$> AP.take 3)
    _ <- AP.string " -> "
    i <- ((BS.unpack) <$> AP.take 3)

    return (i, (ExprWire i (readOp . BS.unpack $ o) w1 w2 ))


parseInput :: ByteString -> M.Map String Wire
parseInput s =
    let  [a,b] = splitOnBs "\n\n" (BS.init s)
         l = map (parse' parseSimple) (BS.lines a) ++ map (parse' parseExpr) (BS.lines b)
         in M.fromList l


parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s


evalW :: (M.Map String Wire) -> Wire -> Int 
evalW _ (SimpleWire _ i) = i 
evalW m (ExprWire _ op a1 a2) = 
    let e1 = (evalW m (fromJust $ M.lookup a1 m))
        e2 =  (evalW m (fromJust $ M.lookup a2 m))
    in (op' op) e1 e2
    where op' OR = (.|.)
          op' AND = (.&.)
          op' XOR = xor

isOr m s | (Just (ExprWire _ OR _ _)) <- (M.lookup s m)  = True
isOr _ _ = False 

isXorStartWire :: Wire -> Bool
isXorStartWire (SimpleWire _ _ ) = False 
isXorStartWire (ExprWire _ XOR a b) = isStartWires a b 
isXorStartWire _ = False 

isAndStartWire :: Wire -> Bool
isAndStartWire (SimpleWire _ _ ) = False 
isAndStartWire (ExprWire _ AND a b) = isStartWires a b 
isAndStartWire _ = False 

isStartWires :: [Char] -> [Char] -> Bool
isStartWires (a:_) (b:_) = (a == 'x' && b == 'y') || (a == 'y' && b=='x')

-- assuming isXorStartWire
checkXorXor :: M.Map String Wire -> Wire -> Maybe  String
checkXorXor m w =  
    let znode = fromJust $ M.lookup  ('z':(tail (fst $ wArgs w))) m 
        (za1, za2) = wArgs znode 
        in if (za1 == wId w || za2 == wId w) || (wId w == "z00") then Nothing  else Just $ (wId znode) 
                                                                        ++ " should have " 
                                                                        ++ (wId w) 
                                                                        ++ " (XOR) as argument but has " ++ show (wArgs znode)
                                                                        ++ "consider switching to "
                                                                        ++ show (    filter (not . isOr m) [za1, za2])


checkZnode m (SimpleWire _ _) = Nothing
checkZnode m w@(ExprWire nid op a1s a2s)  | (nid == "z00" || nid == "z45") = Nothing
                                          | (head nid /= 'z') = Nothing
                                          | isXorXorS m w (tail nid) = Nothing
                                          | otherwise = Just (nid 
                                                              ++ " is not a proper XorXor node : " 
                                                              ++ show w 
                                                              ++ " switch: "
                                                              ++ show (filter (\w -> isXorXorS m w (tail nid)) (M.elems m)))
 
isXorXorS :: M.Map String Wire -> Wire -> [Char] -> Bool
isXorXorS m (SimpleWire _ _) s = False 
isXorXorS m (ExprWire i XOR a1s a2s) s = isXorStartWire a1 && (tail (fst $ wArgs a1) == s) || isXorStartWire a2 && (tail (fst $ wArgs a2) == s)
      where a1 = fromJust $ M.lookup a1s m
            a2 = fromJust $ M.lookup a2s m 

isXorXorS _ _ _ = False

switch m s1 s2 = 
  let w1 = fromJust $ M.lookup s1 m 
      w2 = fromJust $ M.lookup s2 m   
  in M.insert s1 (setId w2 s1) (M.insert s2 (setId w1 s2) m) 

part1 :: ByteString -> IO Integer
part1 s  = do
  let m = parseInput s
      zs = map (fromJust . flip  M.lookup m) $ sort $ filter (\s -> head s == 'z') $ M.keys m 
  return . toInteger . sum $ zipWith (*) (iterate (*2) 1)  (map (evalW m) zs )

part2 :: ByteString -> IO Integer
part2 s = do 
  let m' = parseInput s  
      m'' = switch m' "z18" "skf"
      m''' = switch m'' "z07" "bjm"
      m'''' = switch m''' "nvr" "wkr"
      m = switch m'''' "z13" "hsw"
      elems' = (M.elems m)
      answer = ["z18", "skf","z07","bjm","nvr","wkr","z13","hsw"]
  -- putStrLn $ intercalate "\n" $ (mapMaybe (checkXorXor m) $ (filter isXorStartWire elems'))
  -- putStrLn $ intercalate "\n" $ (mapMaybe (checkZnode m) elems')
  -- print (intercalate "," $ sort ["z18", "skf","z07","bjm","nvr","wkr","z13","hsw"])
  return . toInteger . sum $ zipWith (*) (map (\c -> ord c - ord '0') $ concat answer) (iterate (*74) 1)

