module AI.Pattern.Internal where

import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Data.Tuple
import Debug.Trace
import Gobang

import qualified Text.Printf as T

type Pattern = Int 

data PatternMatchTree a =  Node {patternsMatch :: PatternScore a, emptyNode :: PatternMatchTree a, allyNode :: PatternMatchTree a} 
                       | MissMatch 

newtype PatternScore a = PatternScore ([Pattern], a)

instance Functor PatternScore where
    fmap f (PatternScore (ps, s)) = PatternScore (ps, f s)

nonePatternScore :: (Num a) => PatternScore a
nonePatternScore = PatternScore ([], 0)

psTuple :: PatternScore a -> ([Pattern], a)
psTuple (PatternScore tuple) = tuple

score :: PatternScore a -> a
score (PatternScore (_, s)) = s

fromChar :: Char -> Maybe Pattern
fromChar = fromInt . digitToInt

fromInt :: Int -> Maybe Pattern
fromInt 0 = Just 0
fromInt 1 = Just 1
fromInt _ = Nothing

fromInts :: [Int] -> Maybe [Pattern]
fromInts [] = Nothing
fromInts ints = mapM fromInt ints

fromString :: String -> Maybe [Pattern]
fromString "" = Nothing
fromString s = mapM fromChar s

chessesJudge :: (Num a) => [Chess] -> ([[Int]], a)
chessesJudge cs = let
    (thisPs, thisS) = foldPS $ chessesJudgeSingle cs
    (unswapSwapPs, unswapSwapS) = foldPS $ chessesJudgeSingle (map swapC cs)
    (swapPs, swapS) = (map (map swapC) swapPs, -swapS)
    in (thisPs ++ swapPs, thisS + swapS)

swapC :: Chess -> Chess
swapC 1 = 2
swapC 2 = 1
swapC x = x

foldPS :: (Num a) => [PatternScore a] -> ([[Int]], a)
foldPS pss = foldr step ([], 0) pss where
    step (PatternScore (ops, os)) (ps, s) = (ops:ps, s + os)

chessesJudgeSingle :: (Num a) => [Chess] -> [PatternScore a]
chessesJudgeSingle cs = let 
    css = splitChesses cs
    in concatMap chessesJudgeWithoutSplit css

chessesJudgeWithoutSplit :: (Num a) => [Chess] -> [PatternScore a]
chessesJudgeWithoutSplit = undefined
--chessesJudgeWithoutSplit cs = fst $ fromJust $ find (null . snd) $ iterate iterMatch (nonePatternScore, cs) 

iterMatch :: (Num a) => (PatternScore a, [Chess]) -> (PatternScore a, [Chess])
iterMatch = undefined
--iterMatch PatternScore (scoreSum, cs) = let
--    (nScoreSum, ncs) = match cs
--    in (scoreSum + nScoreSum, ncs)




patternStrs :: (Num a) => [(String, a)]
patternStrs = 
    [ ("11111" , 1000)
    , ("011110", 200)
    , ("01111" , 100)
    , ("10111" , 100)
    , ("11011" , 100)
    , ("01110" , 80) 
    , ("011010", 70)
    , ("0111"  , 40)
    , ("01101" , 35)
    , ("01011" , 34)
    , ("0110"  , 10)
    , ("01010" , 9)
    , ("011"   , 6)
    , ("0101"  , 5)
    ]
patternCases :: (Num a) => [PatternScore a]
patternCases = makeSymmetric $ flip map patternStrs $ \(p, s) -> PatternScore (fromJust (fromString p), s)

makeSymmetric :: [PatternScore a] -> [PatternScore a]
makeSymmetric pss = concat $ flip map pss $ \(PatternScore (ps, s)) -> let
    rps = reverse ps
    in if equalList rps ps
       then [PatternScore (ps, s)]
       else [PatternScore (ps, s), PatternScore (rps, s)]

equalList :: (Eq a) => [a] -> [a] -> Bool
equalList x y = length x == length y && isInfixOf x y 


splitChesses :: [Chess] -> [[Chess]]
splitChesses [] = []
splitChesses cs = 
    let 
    (l,r) = span (<= 1) cs
    (nl, nr) = span (> 1) r
    in filter (/= []) $ l : splitChesses nr



maxScore :: (Ord a, Num a) => [PatternScore a] -> PatternScore a 
maxScore [] = nonePatternScore
maxScore ns = maximumBy (comparing (snd . psTuple)) ns



createTree :: (Ord a, Num a) => [PatternScore a] -> PatternMatchTree a
createTree [] = MissMatch
createTree pss = let
    (emptys, others) = partition firstEmpty pss
    (nones, allys) = partition isNone others
    firstEmpty (PatternScore (ps, s)) = ps /= [] && head ps == 0
    isNone (PatternScore ([], s)) = True
    isNone (PatternScore (_, s)) = False
    maxNone = maxScore nones
    tailPs (PatternScore (ps, s)) = PatternScore (tail ps, s) 
    cts = createTree . map tailPs
    in Node maxNone (cts emptys) (cts allys)
    
matchTreeCase = createTree patternCases 

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy com x y = if com x y == LT 
                then y
                else x

matchByTree :: (Num a, Ord a) => PatternMatchTree a -> [Chess] -> (PatternScore a, [Chess])
matchByTree MissMatch cs = (nonePatternScore, cs)
matchByTree node [] = (patternsMatch node, [])
matchByTree node (c:cs) = let
    thisR = (patternsMatch node, c:cs)
    ms f = let 
        r = matchByTree (f node) cs
        in maxBy (comparing (snd . psTuple . fst)) r thisR 
    in if c == 0
       then ms emptyNode
       else ms allyNode

match = matchByTree matchTreeCase

