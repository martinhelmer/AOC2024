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
    ]

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
f args = timeItT (sToR $ head args)

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

runmany :: Int -> RunMe -> IO ((Double, Integer), (Double, Integer))
runmany n env = do
    (p1, p2) <- let go ((t1, _), (t2, _)) _ = do
                        (t1', r1') <- timeItT (exec 1 $ runMeExecInfo env)
                        (t2', r2') <- timeItT (exec 2 $ runMeExecInfo env)
                        return ((t1 + t1', r1'), (t2 + t2', r2'))
                in foldM go ((0.0, 0), (0.0, 0)) [1 .. n]
    let divn  =  first ( / fromIntegral n)
        in pure ( divn p1, divn p2)
