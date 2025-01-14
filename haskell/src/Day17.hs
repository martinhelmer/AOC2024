{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day17 (runme, runex, runhel) where

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
  string,
  sepBy
 )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, Pos, Dir, tp)
import qualified BSArray as BSA
import Data.Graph.Inductive.Internal.Queue (queuePutList)
import Data.Bits (xor, Bits (shiftL), shiftR)
import Data.List (intersperse)
import Debug.Trace (trace)
import Data.Char (intToDigit, digitToInt)


x :: ByteString
x = ""


part1s :: State
part1s = State Running 60589763 0 0 0 [2,4,1,5,7,5,1,6,4,1,5,5,0,3,3,0] ""

-- Register A: 60589763
-- Register B: 0
-- Register C: 0

-- Program: 2,4,1,5,7,5,1,6,4,1,5,5,0,3,3,0

runex :: RunMe
runex =
  runMeByteString
    "Day 17 - example"
    (return "")
    part1
    (Nothing)
    part2
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 17: Chronospatial Computer ---"
    (readInpByteSTring "day17.txt")
    part1
    (Just 350151510)
    part2
    (Just 107413700225434)


runhel :: RunMe
runhel =
  runMeByteString
    "--- Day 17: Chronospatial Computer  (helfamilj input)---"
    (readInpByteSTring "day17helfamilj.txt")
    part1
    (Just 503576154)
    part2
    (Just 164516454365621)

---
-- State Running 729 0 0 0 [0,1,5,4,3,0] ""
example :: ByteString
example = [r|Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
|]

parseComp :: Parser State
parseComp = let parseRegister s = string s *> decimal <* endOfLine in
      do
        a <- parseRegister "Register A: "
        b <- parseRegister "Register B: "
        c <- parseRegister "Register C: "
        _ <- endOfLine
        p <- string "Program: " *> decimal `sepBy` "," <* endOfLine
        return $ State Running a b c 0 p ""


type Register = Int
type Pointer = Int
type Program = [Int]

data Instructions = ADV | BXL | BST | JNZ | BXZ | OUT | BDV | CDV deriving (Enum, Ord, Eq, Show)

data Status = Running | Stopped deriving (Eq, Show)

data State = State { status :: !Status,
                     regA :: !Register,
                     regB :: !Register,
                     regC :: !Register,
                     instr :: !Pointer,
                     prog :: !Program,
                     output :: !String
                    } deriving (Show)

cycleS :: State -> State
cycleS s
    | i >= length (prog s) = s {status = Stopped}
    | otherwise = case opcode of
        ADV -> s { regA = (regA s `shiftR` (combo operand)), instr = i + 2  }
        BXL -> s { regB = regB s `xor` operand, instr = i + 2}
        BST -> s { regB = combo operand `mod` 8, instr = i + 2 }
        JNZ -> s { instr = if regA s == 0 then i + 2 else operand }
        BXZ -> s { regB = regB s `xor` regC s, instr = i + 2}
        OUT -> s { output = (output s)++show (combo operand `mod` 8),instr = i + 2 }
        -- OUT -> s { output = intToDigit (combo operand `mod` 8) : (output s), instr = i + 2  }
        BDV -> s { regB = (regA s `shiftR` (combo operand)), instr = i + 2  }
        CDV -> s { regC = (regA s `shiftR` (combo operand)), instr = i + 2  }

-- `quot` (2^(combo operand))

    where i = instr s
          opcode = toEnum $ prog s !! i
          operand = prog s !! (i + 1)
          combo o | o <= 3 = o
          combo 4 = regA s
          combo 5 = regB s
          combo 6 = regC s
          combo o = error $ "unknown combo " ++ (show o)


parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

runwith :: State -> String
runwith  = output
          . head 
          . filter ((==) Stopped . status)
          . iterate cycleS 

part1 :: ByteString -> IO Integer
part1 s =
    return
    . read
    . runwith
    $ parse' parseComp s

findlowest :: State -> String -> Int -> (String, Int)
findlowest comp s startwith = let q =  head . dropWhile (\r -> fst r /= s) . map (\x -> (runwith (comp { regA = x}), x)) $ [startwith..] in q

go :: Show a => [a] -> State -> Int
go (x:xs) comp = go' (show x) xs 0 
    where go' match [] startwith = snd $ findlowest comp match startwith
          go' match (x:xs) startwith = let l = snd $ findlowest comp  match startwith in go' (show x ++ match) xs (l `shiftL` 3)
part2 :: ByteString -> IO Integer
part2 s = do
    let comp = parse' parseComp s
    return  . toInteger . go (reverse $ prog comp) $ comp 

-- [2,4,1,5,7,5,1,6,4,1,5,5,0,3,3,0]
-- 107413700225434 !
