{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE LambdaCase #-}

module Day15b (runme, runex) where

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
import AOCHelper (readInpByteSTring, Pos, tp, splitOnBs, parseIntoArray)
import qualified BSArray as BSA
import BSArray (BSArray)
import PosDir (Loc, (.->.), Dir (..))
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Data.List (foldl')
import qualified Data.Array.Unboxed as A
import qualified Data.Array.MArray as MA
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, STUArray, mapArray)
import qualified Data.Array.Base as MS
import Control.Monad (foldM, foldM_, when)
import Control.Monad.Loops (andM)

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
    "-- Day 15b: Warehouse Woes (Marray) ---"
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

type Grid = A.Array (Int,Int) Char
---
parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

readInp :: ByteString -> (ByteString -> ByteString) -> (Loc, Grid, ByteString)
readInp s sub | [a,b] <- splitOnBs "\n\n" (BS.init s) =
                    let grid = parseIntoArray (BS.unpack . sub $ a);
                        sp = fst . head $ filter (\(_,c) -> c == '@') $ A.assocs grid
                    in  (sp,  grid A.// [(sp,'.')] , BS.concat $ BS.lines b)
readInp _ _ =  error "unexpected input structure"

tloc :: Loc -> Char -> Loc
tloc loc c = loc .->. (d c)

gps :: Num a => (a, a) -> a
gps (a,b) = 100 * a + b

d :: Char -> Dir
d  = \case
  '<' ->  WEST
  '>' -> EAST
  '^' -> NORTH
  'v' -> SOUTH
  _ -> undefined

part1 :: ByteString -> IO Integer
part1 s = do
    let (sp, grid, instr) = readInp s id
        rg = runST (dogrid grid sp (BS.unpack instr))

    return . toInteger . sum . map (gps . fst) $ filter (\t -> (snd t) =='O') $ A.assocs rg

part2 :: ByteString -> IO Integer
part2  s = do
    let (sp, grid, instr) = readInp s substitute
    let rg = runST (dogrid grid sp (BS.unpack instr))
    return . toInteger . sum . map (gps . fst) $ filter (\t -> (snd t) =='[') $ A.assocs rg

----

frz :: STUGrid s -> ST s Grid
frz = MA.freeze

thw :: Grid -> ST s (STUGrid s)
thw = MA.thaw

---


moveST :: (STUGrid s, Loc) -> Char -> ST s(STUGrid s, Loc)
moveST (grid, loc) c = do
    gg <- push loc (d c) grid
    pure $ case gg of
        Nothing -> (grid, loc)
        Just grid' -> (grid',  (loc .->. (d c)))

dogrid :: Grid -> Loc -> String -> ST s (Grid)
dogrid grid sp instr =
  thw grid >>= \g -> foldM moveST (g, sp) instr >>= frz . fst


substitute :: ByteString -> ByteString
substitute = BS.pack . sub' . BS.unpack
    where sub' [] = []
          sub' ('.':xs) = '.':'.':sub' xs
          sub' ('@':xs) = '@':'.':sub' xs
          sub' ('#':xs) = '#':'#':sub' xs
          sub' ('O':xs) = '[':']':sub' xs
          sub' (x:xs) = x:(sub' xs)


type STUGrid s= (STUArray s Loc Char)

push :: Loc -> Dir -> (STUGrid s) -> (ST s (Maybe (STUGrid s)))
push loc dir g = do
    gg <- MA.readArray g nextpos
    case (gg) of
        '.' -> move'' loc nextpos g
        '#' -> pure $ Nothing
        'O' -> pushandmove
        '[' -> movebox EAST
        ']' -> movebox WEST
        _ -> undefined
    where nextpos = loc .->. dir
          movebox d
            | dir == WEST || dir == EAST = pushandmove
            | otherwise = do
                      ispush <- (andM [isPushable nextpos dir g, isPushable (nextpos  .->. d) dir g])                      
                      if ispush then   
                          (pushandmove >>= maybe (pure Nothing) (push (loc .->. d .->. dir ) dir))
                      else 
                          pure Nothing 
          pushandmove =  do
                p <- (push nextpos dir g)
                case p of
                    Nothing -> pure Nothing
                    Just g -> move'' loc nextpos g

-- 
move'' :: Loc -> Loc -> STUGrid s -> (ST s (Maybe (STUGrid s)))
move'' from to grid' = do
      fromv <- MA.readArray grid' from
      if fromv == '.' then pure $ Just grid' else do
                        MA.writeArray grid' from '.'
                        MA.writeArray grid' to fromv
                        pure $ Just grid'

isPushable :: Loc -> Dir -> (STUGrid s) -> (ST s (Bool))
isPushable loc dir g =
   let nextpos = loc .->. dir in
   MA.readArray g nextpos >>= 
     \case 
        '.' -> pure True
        '#' -> pure False
        'O' -> isPush' nextpos
        '[' -> andM [isPush' nextpos, isPush' (nextpos .->. EAST)]
        ']' -> andM [isPush' nextpos, isPush' (nextpos .->. WEST)]
        _ -> undefined
    where isPush' p = isPushable p dir g 