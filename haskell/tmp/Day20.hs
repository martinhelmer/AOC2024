{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day20 (runme, runex) where

import Text.RawString.QQ

import Data.Char ( ord )
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  char,
  decimal,
  endOfInput,
  endOfLine,
  isDigit,
  many1,
  parseOnly,
  skipSpace,
  skipWhile,
  sepBy1,
 )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Vector (Vector)

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring, Pos, Dir, tp)
import qualified BSArray as BSA
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as SQ
import Data.Sequence ((><))
import Debug.Trace (trace)
import qualified Data.Graph.Inductive as M

type Map a b= Map.HashMap a b
type Set a = Set.HashSet a
runex :: RunMe
runex =
  runMeByteString
    "Day 20 - example"
    (return example)
    part1
    (Just 11687500)
    part2
    Nothing

runme :: RunMe
runme =
  runMeByteString
    "Day 20: Pulse Propagation"
    (readInpByteSTring "day20.txt")
    part1
    (Just 812721756)
    part2
    (Just 233338595643977)

---
example :: ByteString
example =
  [r|broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> rx|]
---
parseModule :: Int -> Parser Module
parseModule ix = do
   t <- AP.option Broadcaster ( FlipFlop False <$ char '%' <|> Conjunction Set.empty <$ char '&')
   id' <- AP.takeWhile (AP.inClass "a-z")
    <*  AP.string " -> "
   out <- AP.takeWhile (AP.inClass "a-z") `sepBy1` AP.string ", "
   return $ Module t id' ix out []

parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s
---
makemap :: [Module] -> Map ByteString Module
makemap l = foldl  f  inimap  (Map.elems inimap)
  where
    inimap = Map.fromList $ map (\m -> (moduleId m,m)) (output:l)
    f :: Map ByteString Module -> Module ->  Map ByteString Module
    f m moduleToAdd = foldl (flip (Map.adjust (\m' -> m' {moduleInputs = moduleId moduleToAdd:moduleInputs m'} ))) m (moduleOutputs moduleToAdd)

-- 

data ModuleType = Button | Broadcaster | FlipFlop !Bool | Conjunction !(Set ByteString) | Output deriving (Eq, Show)

data Module = Module
  { moduleType :: !ModuleType
  , moduleId :: !ByteString
  , moduleIx :: Int
  , moduleOutputs :: ![ByteString]
  , moduleInputs :: ![ByteString]
  } deriving (Show)

data Pulse = Pulse { source :: !ByteString
                     , dest :: !ByteString
                     , strength :: !Bool
                     } deriving (Show)
button :: Module
button = Module Button "button" 0 ["broadcaster"] []

output :: Module
output = Module Output "rx" 63 [] []

startPulse :: Pulse
startPulse = Pulse "button" "broadcaster" False


startQueue :: SQ.Seq Pulse
startQueue = SQ.singleton startPulse

updReceiver :: Module -> Pulse -> Module
updReceiver mod'@(Module (Conjunction set) _ _ _ _) signal =  mod' { moduleType = Conjunction updatedSet }
  where updatedSet = if strength signal then Set.insert (source signal) set else Set.delete (source signal) set
updReceiver mod'@(Module (FlipFlop s) _ _ _ _) (Pulse _ _ False)   = mod' { moduleType = FlipFlop (not s)}
updReceiver mod' _ = mod'

doPulse :: Module -> Pulse -> (Module, [Pulse])
doPulse m p = let updatedModule = updReceiver m p in (updatedModule, sendPulse updatedModule p)

sendPulse :: Module -> Pulse -> [Pulse]
sendPulse m (Pulse _ _ inSignal) =
  case moduleType m of
    Button -> outs False -- button sends low
    Broadcaster -> outs inSignal
    Conjunction set -> outs (Set.size set /= length (moduleInputs m) )
    FlipFlop state -> if inSignal then [] else outs state
    _ -> []
 where
  outs s = map (\d -> Pulse (moduleId m) d s) (moduleOutputs m)

part1 :: ByteString -> IO Integer
part1 s = do
  let mymap = makemap (map (parse' (parseModule 1) ) (BS.lines s))
  let after1000 = iterate pressButton (Context 0 0 0 Map.empty SQ.empty mymap ) !! 1000
  return . toInteger $ highs after1000 * lows after1000

part2 :: ByteString -> IO Integer
part2 s = do
  let mymap = makemap (map (parse' (parseModule 1) ) (BS.lines s))
  let afterp2 = pressButtonUntil (Context 0 0 0 Map.empty SQ.empty mymap )
  return . toInteger . product $ Map.elems (p2set afterp2)

type Fifo = SQ.Seq Pulse
data Context = Context
                { highs :: Int
                , lows :: Int
                , buttonpresses :: Int
                , p2set :: Map ByteString Int
                , queue :: Fifo
                , states :: Map ByteString Module
                } deriving (Show)

oneCycle:: Context -> Context
oneCycle c@(Context h l b p2s queue' map') =
  case SQ.viewl queue' of -- (trace (show queue ))
    SQ.EmptyL -> c
    pulse SQ.:< rest -> oneCycle (Context h' l' b p2s' (rest <> SQ.fromList pulses)  (Map.insert (moduleId newmod) newmod map') )
      where mod' =  map' Map.! dest pulse
            p2s'  = if moduleId mod' == "lv" && strength pulse then Map.insertWith (\ _ x -> x) (source pulse) (buttonpresses c) p2s else p2s
            (newmod, pulses) = doPulse mod' pulse
            (h',l') = if strength pulse then (h+1, l) else (h, l+1)

pressButton :: Context -> Context
pressButton c = oneCycle (c {buttonpresses = buttonpresses c +1, queue = startQueue})

pressButtonUntil :: Context-> Context
pressButtonUntil c =
    case Map.size $ p2set cycle' of
        4 -> cycle'
        _ -> pressButtonUntil cycle'
    where cycle' = oneCycle (c {buttonpresses = buttonpresses c +1, queue = startQueue})
