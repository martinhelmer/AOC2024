{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad
-- import Data.Time.Clock
import System.TimeIt
import Data.Bifunctor (first)
import Data.Text.Format.Numbers (prettyI )
import qualified Data.Text as T
import Data.Text(Text)
import Data.Text.IO as TI
import qualified Data.Map as M
import System.Console.Pretty (Color (..), color, Pretty)
import RunUtil (RunMe(..), exec)

import Day01 
import Day02 
import Day03
import Day04 
import Day05 
import Day06
import Day07 
import Day08 
-- import Day09 
import Day09b 
import Day10 
import Day11 
import Day11b 
import Day12
import Day13 
import Day14 
import Day15 
import Day16 
import Day17 
import Day18 
import Day19
import Day20
import Day21
import Day22
import Day22b 
import Day23
import Day24 
import Day25 
import Data.Time (getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)

type RunResult = ((Double, Integer), (Double, Integer))
type Runner = RunMe-> IO RunResult
type Shower = RunMe -> RunResult -> IO Text

rmap :: M.Map String RunMe
rmap = M.fromList
    [ ("01", Day01.runme)
    , ("_01", Day01.runex)
    , ("02", Day02.runme)
    , ("_02", Day02.runex)
    , ("03", Day03.runme)
    , ("_03", Day03.runex)
    , ("04", Day04.runme)
    , ("_04", Day04.runex)
    , ("05", Day05.runme)
    , ("_05", Day05.runex)
    , ("06", Day06.runme)
    , ("_06", Day06.runex)
    , ("07", Day07.runme)
    , ("_07", Day07.runex)
    , ("08", Day08.runme)
    , ("_08", Day08.runex)    
    , ("09", Day09b.runme)
    , ("_09", Day09b.runex)  
    , ("10", Day10.runme)
    , ("_10", Day10.runex)  
    , ("11", Day11.runme)
    , ("_11", Day11.runex)        
    , ("11b", Day11b.runme)
    , ("_11b", Day11b.runex)   
    , ("12", Day12.runme)
    , ("_12", Day12.runex) 
    , ("13", Day13.runme)
    , ("_13", Day13.runex)
    , ("14", Day14.runme)              
    , ("_14", Day14.runex)       
    , ("15", Day15.runme)              
    , ("_15", Day15.runex) 
    , ("16", Day16.runme)              
    , ("_16", Day16.runex) 
    , ("17", Day17.runme)              
    , ("_17", Day17.runex)
    , ("_17h", Day17.runhel)                
    , ("18", Day18.runme)
    , ("_18", Day18.runex)
    , ("19", Day19.runme)
    , ("_19", Day19.runex)
    , ("20", Day20.runme)
    , ("_20", Day20.runex)
    , ("21", Day21.runme)
    , ("_21", Day21.runex)
    , ("22", Day22.runme)
    , ("_22", Day22.runex)
    , ("22b", Day22b.runme)
    , ("_22b", Day22b.runex)
    , ("23", Day23.runme)
    , ("_23", Day23.runex)
    , ("24", Day24.runme)
    , ("_24", Day24.runex)
    , ("25", Day25.runme)
    , ("_25", Day25.runex)    ]

numruns :: Int
numruns = 1

doit :: Runner-> [RunMe] -> Shower -> IO ()
doit runner jobs shower = forM_ jobs ( \job -> runner job >>= shower job >>= TI.putStrLn )

mySort :: [(String, b)] -> [(String, b)]
mySort = sortBy (compare `on` fst)

sToR :: String -> IO ()
sToR "ALL" = do
        _ <- doit (runmany 1) [Day03.runme] showNothing
        TI.putStrLn "Haskell AOC 2024                         |  1    (micros)    2 |   1 + 2  |"
        TI.putStrLn "-----------------------------------------+----------+----------+----------+--"
        doit (runmany numruns) (map snd . filter (\t -> '_' `notElem` fst t ) . mySort . M.assocs $ rmap) showTabular
        TI.putStrLn "-----------------------------------------+----------+----------+----------+--"
sToR s = doit (runmany 1) [rmap M.! s] showVerbose

f :: [String] -> IO (Double, ())
f args = timeItT' (sToR $ head args)

main :: IO ()
main = do
    args <- getArgs
    f args >>= \t -> TI.putStrLn $
                        "                                                        "
                        <> color Blue "Total:"
                        <> " |"
                        <> color Blue (showDur 9 (fst t / fromIntegral numruns))
                        <> " |"

---

showDur :: Int -> Double -> Text
showDur n  = T.justifyRight n ' ' . prettyI (Just '\'') . floor . (1000_000.0 *)

showDur' :: Eq a => Int -> a -> Maybe a -> Double -> Text
showDur' n r x = applyColor r x . showDur n

showNothing :: RunMe -> ((Double, Integer), (Double, Integer)) -> IO Text
showNothing _ _ = pure "Warming up ... \n"

showTabular :: RunMe -> ((Double, Integer), (Double, Integer)) -> IO Text
showTabular env ((t1, r1), (t2, r2)) = pure $  T.intercalate " |"
    [ T.justifyLeft 40 ' ' . T.pack . runMeTitle $ env
    , showDur' 9 r1 (runMeExpected1 env) t1
    , showDur' 9 r2 (runMeExpected2 env) t2
    , showDur 9 (t1 + t2)
    , ""
    ]

applyColor :: (System.Console.Pretty.Pretty p, Eq a) => a -> Maybe a -> p -> p
applyColor _ Nothing s = s
applyColor r (Just i) s = color (if i == r then Green else Red ) s

showResult :: (Eq a, Show a) => Int -> Maybe a -> a -> Text
showResult n x r = applyColor r x . T.justifyRight n ' ' . T.pack . show $ r

showVerbose :: RunMe -> ((Double, Integer), (Double, Integer)) -> IO Text
showVerbose env ((t1, r1), (t2, r2)) = do
    pure $ T.intercalate (T.pack "\n")
        [T.pack (runMeTitle env)
        , sr "Part 1" r1 (runMeExpected1 env) t1
        , sr "Part 2" r2 (runMeExpected2 env) t2
        , "                                           Both: " <> showDur 9 (t1 + t2)
        ]
        where sr p r xp t = T.unwords [ p,T.pack " = ", showResult 10 xp r , " (", T.justifyRight 15 ' ' . T.pack . show $ xp, ") dur:", showDur 10 t]

timeItT' :: IO b -> IO (Double, b)
timeItT' f = do 
    s <- getCurrentTime
    a <- f
    e <- getCurrentTime

    return (realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime e s, a) 

runmany :: Int -> RunMe -> IO ((Double, Integer), (Double, Integer))
runmany n env = do
    (p1, p2) <- let go ((t1, _), (t2, _)) _ = do
                        (t1', r1') <- timeItT' (exec 1 $ runMeExecInfo env)
                        (t2', r2') <- timeItT' (exec 2 $ runMeExecInfo env)
                        return ((t1 + t1', r1'), (t2 + t2', r2'))
                in foldM go ((0.0, 0), (0.0, 0)) [1 .. n]
    let divn  =  first ( / fromIntegral n)
        in pure ( divn p1, divn p2)
