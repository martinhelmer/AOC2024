{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Day19 (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import Control.Monad (join)
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  char,
  decimal,
  inClass,
  satisfy,
 )
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)

import AOCHelper (readInpByteSTring)
import Data.Map (Map, (!))
import qualified Data.Map as M
import RunUtil (RunMe, runMeByteString)
import Data.Ord (Ordering(..))
runex :: RunMe
runex =
  runMeByteString
    "Day 19 - example"
    (return example)
    part1
    (Just 19114)
    part2
    (Just 167409079868000)

runme :: RunMe
runme =
  runMeByteString
    "Day 19: Aplenty"
    (readInpByteSTring "day19.txt")
    part1
    (Just 425811)
    part2
    (Just 131796824371749)

---

example :: ByteString
example =
  [r|px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
|]

-------------------------

data ARef = Acc | Rej | Ref ByteString deriving (Show)

----
ordering :: Parser Ordering
ordering = (GT <$ char '>' ) <|> (LT <$ char '<')
newline :: Parser Char
newline = AP.char '\n'

aref :: Parser ARef
aref = (Acc <$ char 'A') <|> (Rej <$ char 'R') <|> Ref <$> AP.takeWhile (inClass "a-z")

parseCond :: Parser (Condition, ARef)
parseCond =
  (,)
    <$> ( Condition
            <$> satisfy (inClass "xmas")
            <*> satisfy (inClass "<>")
            <*> decimal
        )
    <* char ':'
    <*> aref

parseAtom :: Parser (Either (Condition, ARef) ARef)
parseAtom = AP.eitherP parseCond aref

type EvalRow = (ByteString, [Either (Condition, ARef) ARef])

parseEvalRow :: Parser EvalRow
parseEvalRow = (,) <$> header  <* char '{'  <*> body  <* char '}'
 where
  header = AP.takeWhile AP.isAlpha_ascii
  body = parseAtom `AP.sepBy` char ','

-- {x=787,m=2655,a=1222,s=2876}
parseRangeRow :: Parser RatingRange
parseRangeRow =
  RatingRange
    <$> skip3dec
    <*> skip3dec
    <*> skip3dec
    <*> skip3dec
    <* char '}'
 where
  skip3dec = (\x -> (x, x)) <$> (AP.take 3 *> decimal)

parse' :: Parser b -> ByteString -> b
parse' p s = either (error . show) id $ AP.parseOnly (p <* AP.endOfInput) s

parseInput :: Parser (EvalTree, [RatingRange])
parseInput =
  (,)
    <$> ( flip buildEvalTree [Right (Ref "in")] . M.fromList
            <$> AP.many1 (parseEvalRow <* newline)
        )
    <* newline
    <*> AP.many1 (parseRangeRow <* newline)

----

data EvalTree = Accepted | Rejected | Node Condition EvalTree EvalTree deriving (Show)

data Condition = Condition Char Char Int deriving (Show)

data RatingRange = RatingRange
  { getxr :: (Int, Int)
  , getmr :: (Int, Int)
  , getar :: (Int, Int)
  , getsr :: (Int, Int)
  }
  deriving (Show)

buildEvalTree :: Map ByteString [Either (Condition, ARef) ARef] -> [Either (Condition, ARef) ARef] -> EvalTree
buildEvalTree _ [Right Acc] = Accepted
buildEvalTree _ [Right Rej] = Rejected
buildEvalTree m (Right (Ref s) : _) = buildEvalTree m (m ! s)
buildEvalTree m (Left (c, ref) : xs) = Node c (buildEvalTree m [Right ref]) (buildEvalTree m xs)
buildEvalTree _ l = error ("Buildevaltree called with : " <> show l)

getr :: Char -> RatingRange -> (Int, Int)
getr c = case c of
  'x' -> getxr
  'm' -> getmr
  'a' -> getar
  's' -> getsr
  _ -> error "unsupported range attribute"

setr :: Char -> RatingRange -> (Int, Int) -> RatingRange
setr c rr nr = case c of
  'x' -> rr{getxr = nr}
  'm' -> rr{getmr = nr}
  'a' -> rr{getar = nr}
  's' -> rr{getsr = nr}
  _ -> error "unsupported range attribute"

-------------------------

evalrr :: RatingRange -> EvalTree -> [RatingRange]
evalrr rr Accepted = [rr]
evalrr _ Rejected = []
evalrr rr (Node cond matchE nomatchE) =
  case checkrr rr cond of
    (Just match, Just nomatch) -> evalrr match matchE <> evalrr nomatch nomatchE
    (Just match, Nothing) -> evalrr match matchE
    (Nothing, Just nomatch) -> evalrr nomatch nomatchE
    _ -> error "(evalrr) Should not be here"

checkrr :: RatingRange -> Condition -> (Maybe RatingRange, Maybe RatingRange)
checkrr rr (Condition field cmp lim) = join bimap (setr field rr <$>) splits
 where
  rangeToCheck = getr field rr
  splits = splitr cmp lim rangeToCheck -- (Maybe (Int, Int), Maybe (Int, Int))

splitr :: Char -> Int -> (Int, Int) -> (Maybe (Int, Int), Maybe (Int, Int))
splitr cmp v r@(st, en) = case cmp of
  '<' -> if en < v then (Just r, Nothing) else if v <= st then (Nothing, Just r) else (Just (st, v - 1), Just (v, en))
  '>' -> if st > v then (Just r, Nothing) else if v >= en then (Nothing, Just r) else (Just (v + 1, en), Just (st, v))
  _ -> error ("splitr called with Char = " <> show cmp)

--

partSum :: RatingRange -> Int
partSum (RatingRange (a, _) (b, _) (c, _) (d, _)) = a + b + c + d

part1 :: ByteString -> IO Integer
part1 s = do
  let (evaltree, parts) = parse' parseInput s
  return
    . toInteger
    . sum
    . map partSum
    . concatMap (`evalrr` evaltree)
    $ parts

partComb :: RatingRange -> Integer
partComb (RatingRange a b c d) = toInteger . product . map (\(x, x') -> x' - x + 1) $ [a,b,c,d]

part2 :: ByteString -> IO Integer
part2 s = do
  let (evaltree, _) = parse' parseInput s
  return
    . sum
    . map partComb
    $ evalrr (RatingRange (1, 4000) (1, 4000) (1, 4000) (1, 4000)) evaltree