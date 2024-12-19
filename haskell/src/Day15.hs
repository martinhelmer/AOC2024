{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE LambdaCase #-}

module Day15 (runme, runex) where

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
import AOCHelper (readInpByteSTring, Pos, tp, splitOnBs)
import qualified BSArray as BSA
import BSArray (BSArray)
import PosDir (Loc, (.->.), Dir (..))
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import qualified Data.ByteString as Bs

runex :: RunMe
runex =
  runMeByteString
    "Day 15 - example"
    (return example)
    part1
    (Just 10092)
    part2
    (Just 9021)

runme :: RunMe
runme =
  runMeByteString
    "--- Day 15: Warehouse Woes ---"
    (readInpByteSTring "day15.txt")
    part1
    (Just 1451928)
    part2
    (Just 1462788)

example :: ByteString
example =
  [r|##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
|]

example1 :: ByteString
example1 =
  [r|########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
|]
---
parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

readInp :: ByteString -> (ByteString -> ByteString) -> (Loc, BSA.BSArray, ByteString)
readInp s sub | [a,b] <- splitOnBs "\n\n" (BS.init s) =
                    let grid = BSA.makeBSarray (sub a);
                        sp = fromJust $ BSA.elemIndex grid '@'
                    in  (sp, BSA.updateLoc grid sp '.' , BS.concat $ BS.lines b)
readInp _ _ =  error "unexpected input structure"

move :: (BSArray, Loc) -> Char -> (BSArray, Loc)
move (grid, loc) c = let nextpos = tloc loc c in
            case BSA.unsafeLookup grid (nextpos) of
                '#' -> (grid, loc)
                '.' -> (grid, nextpos)
                'O' -> case (spot nextpos) of
                            Nothing -> (grid, loc)
                            Just l -> ( BSA.updateLoc (BSA.updateLoc grid l 'O') nextpos '.', nextpos)
                _ -> error "ERROR"
        where spot loc'  = case (BSA.unsafeLookup grid loc') of
                            '#' -> Nothing
                            '.' -> Just loc'
                            'O' -> spot (tloc loc' c)
                            _  -> error "eehhmm"

tloc :: Loc -> Char -> Loc
tloc loc c = loc .->. (d c)

gps :: Num a => (a, a) -> a
gps (a,b) = 100 * a + b

-- part1 :: ByteString -> IO Integer
-- part1 s = do 
--     let (sp, grid, instr) = readInp s id
--     let (rg, _) = foldr (flip move) (grid, sp) (reverse $ BS.unpack instr)
--     return . toInteger . sum . map gps . BSA.elemIndices 'O' $ rg
d :: Char -> Dir
d  = \case 
  '<' ->  WEST
  '>' -> EAST
  '^' -> NORTH
  'v' -> SOUTH
  _ -> undefined

move' :: (BSArray, Loc) -> Char -> (BSArray, Loc)
move' (grid, loc) c = 
    case push loc (d c) grid of
          Nothing -> (grid, loc)
          Just grid' -> (grid' , (loc .->. (d c)))


part1 :: ByteString -> IO Integer
part1 s = do
    let (sp, grid, instr) = readInp s id
    let (rg, _) = foldr (flip move) (grid, sp) (reverse $ BS.unpack instr)
    return . toInteger . sum . map gps . BSA.elemIndices 'O' $ rg

-- 1491474 too high
part2 :: ByteString -> IO Integer
part2  s = do
    let (sp, grid, instr) = readInp s substitute
    -- print (sp)
    -- print (BSA.updateLoc grid sp '@')
    let (rg, rp) = foldr ( flip move') (grid, sp) (reverse $ BS.unpack instr)
    -- print ("---")
    -- print (BSA.updateLoc rg rp '@')
    return . toInteger . sum . map gps . BSA.elemIndices '[' $ rg

substitute :: ByteString -> ByteString
substitute = BS.pack . sub' . BS.unpack
    where sub' [] = []
          sub' ('.':xs) = '.':'.':sub' xs
          sub' ('@':xs) = '@':'.':sub' xs
          sub' ('#':xs) = '#':'#':sub' xs
          sub' ('O':xs) = '[':']':sub' xs
          sub' (x:xs) = x:(sub' xs)

push :: Loc -> Dir -> BSArray -> Maybe BSArray
push loc dir grid = case (BSA.lookup grid nextpos) of
        '.' -> Just ( move'' loc nextpos grid)
        '#' -> Nothing
        'O' -> pushandmove
        '[' -> if  dir `elem` [WEST, EAST] then pushandmove else (pushandmove) >>= (push (loc .->. EAST .->. dir ) dir)
        ']' -> if  dir `elem` [WEST, EAST] then pushandmove else (pushandmove) >>= (push (loc .->. WEST .->. dir ) dir)
        _ -> undefined
    where move'' from to grid' = BSA.updateLoc (BSA.updateLoc grid' from '.') to (BSA.lookup grid' from)
          pushandmove =  (move'' loc nextpos) <$> (push nextpos dir grid) 
          nextpos = loc .->. dir
  