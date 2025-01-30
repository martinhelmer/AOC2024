{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day15b (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, splitOnBs, parseIntoArray)
import PosDir (Loc, (.->.), Dir (..))
import qualified Data.Array.Unboxed as A
import qualified Data.Array.MArray as MA
import Data.Array.MArray (thaw, freeze)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray)
import Control.Monad (foldM, when)

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
    "-- Day 15b: WarehouseWoes (Marr)"
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
type STUGrid s= (STUArray s Loc Char)

---

readInp :: ByteString -> (ByteString -> ByteString) -> (Loc, Grid, [Dir])
readInp s sub | [a,b] <- splitOnBs "\n\n" (BS.init s) =
                    let grid = parseIntoArray (BS.unpack . sub $ a);
                        sp = fst . head $ filter (\(_,c) -> c == '@') $ A.assocs grid
                    in  (sp,  
                         grid A.// [(sp,'.')] , 
                         map d . BS.unpack $ BS.concat $ BS.lines b)
readInp _ _ =  error "unexpected input structure"

gps :: (Int, Int) -> Int
gps (a,b) = 100 * a + b

d :: Char -> Dir
d  = \case
  '<' ->  WEST
  '>' -> EAST
  '^' -> NORTH
  'v' -> SOUTH
  _ -> undefined

-- 

part1 :: ByteString -> IO Integer
part1  = go id 'O'

part2 :: ByteString -> IO Integer
part2 = go  substitute '['

substitute :: ByteString -> ByteString
substitute = BS.pack . sub' . BS.unpack
    where sub' [] = []
          sub' ('.':xs) = '.':'.':sub' xs
          sub' ('@':xs) = '@':'.':sub' xs
          sub' ('#':xs) = '#':'#':sub' xs
          sub' ('O':xs) = '[':']':sub' xs
          sub' (x:xs) = x:(sub' xs)


go ::  (ByteString -> ByteString) -> Char -> ByteString-> IO Integer
go sub gpsOnChar  s= do
    let (sp, grid, instr) = readInp s sub
    return . toInteger 
           . sum 
           . map (gps . fst) 
           . filter (\t -> (snd t) == gpsOnChar) 
           . A.assocs 
           $ (runST 
                  (thaw grid >>=
                   (\g -> foldM moveST (g,sp) instr) >>=
                   freeze . fst
                  ):: Grid)

moveST :: (STUGrid s, Loc) -> Dir -> ST s(STUGrid s, Loc)
moveST (grid, loc) dir = do
    (\gg -> (grid, if gg then (loc .->. dir) else loc))
    <$> push loc dir grid

push :: Loc -> Dir -> (STUGrid s) -> (ST s (Bool))
push loc dir g = do
    gg <- MA.readArray g nextpos
    case (gg) of
        '.' -> doMove loc nextpos g
        '#' -> pure False
        'O' -> pushandmove
        '[' -> movebox EAST
        ']' -> movebox WEST
        _ -> undefined
    where nextpos = loc .->. dir
          movebox otherside
            | dir == WEST || dir == EAST = pushandmove
            | otherwise = andM2
                          (andM2 (isPushable nextpos dir g)
                                 (isPushable (nextpos  .->. otherside) dir g))
                          (andM2 pushandmove
                                 (push (nextpos .->. otherside ) dir g))
          pushandmove =  andM2 (push nextpos dir g) (doMove loc nextpos g)

doMove :: Loc -> Loc -> STUGrid s -> (ST s (Bool))
doMove from to grid' = do
      fromv <- MA.readArray grid' from
      when (fromv /= '.' ) (do
          MA.writeArray grid' from '.'
          MA.writeArray grid' to fromv)
      pure True

isPushable :: Loc -> Dir -> (STUGrid s) -> (ST s (Bool))
isPushable loc dir g =
   let nextpos = loc .->. dir in
   MA.readArray g nextpos >>=
     \case
        '.' -> pure True
        '#' -> pure False
        'O' -> isPush' nextpos
        '[' -> andM2 (isPush' nextpos) (isPush' (nextpos .->. EAST))
        ']' -> andM2 (isPush' nextpos) (isPush' (nextpos .->. WEST))
        _ -> undefined
    where isPush' p = isPushable p dir g

andM2 :: Monad m => m Bool -> m Bool -> m Bool
andM2 x y = do
  a <- x
  if a then y else pure False