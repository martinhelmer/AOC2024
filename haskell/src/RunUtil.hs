{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module RunUtil (runMeString, runMeText, runMeByteString, RunMe (..), ArgsnParts (..), exec) where

import Data.ByteString (ByteString)
import Data.Text (Text)

data ArgsnParts
        = APString (IO String) (String -> IO Integer) (String -> IO Integer)
        | APText (IO Text) (Text -> IO Integer) (Text -> IO Integer)
        | APBS (IO ByteString) (ByteString -> IO Integer) (ByteString -> IO Integer)
        

data RunMe = RunMe
        { runMeTitle :: String
        , runMeExecInfo :: ArgsnParts
        , runMeExpected1 :: Maybe Integer
        , runMeExpected2 :: Maybe Integer
        } 

runMeString ::
        String ->
        IO String ->
        (String -> IO Integer) ->
        Maybe Integer ->
        (String -> IO Integer) ->
        Maybe Integer ->
        RunMe
runMeString title arg p1 e1 p2 e2 =
        RunMe title (APString arg p1 p2) e1 e2

runMeText :: String -> IO Text -> (Text -> IO Integer) -> Maybe Integer -> (Text -> IO Integer) -> Maybe Integer -> RunMe
runMeText title arg p1 e1 p2 e2 =
        RunMe title (APText arg p1 p2) e1 e2

runMeByteString :: String -> IO ByteString -> (ByteString -> IO Integer) -> Maybe Integer -> (ByteString -> IO Integer) -> Maybe Integer -> RunMe
runMeByteString title arg p1 e1 p2 e2 =
        RunMe title (APBS arg p1 p2) e1 e2

exec :: Int -> ArgsnParts -> IO Integer
exec 1 (APString arg part1 _) = arg >>= part1 >>= forrp  
exec 2 (APString arg _ part2) = arg >>= part2 >>= forrp  
exec 1 (APText arg part1 _) = arg >>= part1 >>= forrp  
exec 2 (APText arg _ part2) = arg >>= part2 >>= forrp  
exec 1 (APBS arg part1 _) = arg >>= part1 >>= forrp  
exec 2 (APBS arg _ part2) = arg >>= part2 >>= forrp  
exec _ _ = undefined

forrp x = if x == 1 then putStrLn "@" >> return x else return  x