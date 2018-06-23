module AI.Pattern.Interal where

import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Data.Tuple
import Debug.Trace
import Gobang

import qualified Text.Printf as T
data Pattern = Empty | Ally | Enemy deriving (Show, Read, Eq)
data PatternMatchTree =  Node {score :: Int, emptyNode :: PatternMatchTree, allyNode :: PatternMatchTree} 
                       | MissMatch 
                       deriving (Show, Read)
type PatternScore = ([Pattern], Int)


fromChar :: Char -> Maybe Pattern
fromChar = fromInt . digitToInt

fromInt :: Int -> Maybe Pattern
fromInt 0 = Just Empty
fromInt 1 = Just Ally
fromInt _ = Nothing

fromInts :: [Int] -> Maybe [Pattern]
fromInts [] = Nothing
fromInts ints = mapM fromInt ints

fromString :: String -> Maybe [Pattern]
fromString "" = Nothing
fromString s = mapM fromChar s

patternStrs = 
    [ ("11111" , 1000)
    , ("011110", 200)
    , ("01111" , 100)
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
patternCases :: [PatternScore]
patternCases = flip map patternStrs $ \(p, s) -> (fromJust (fromString p), s)

symmetricPatternCases = makeSymmetric patternCases

swapC :: Chess -> Chess
swapC 1 = 2
swapC 2 = 1
swapC x = x
chessesJudge :: [Chess] -> Int 
chessesJudge cs = chessesJudgeSingle cs - chessesJudgeSingle (map swapC cs)

splitChesses :: [Chess] -> [[Chess]]
splitChesses [] = []
splitChesses cs = 
    let 
    (l,r) = span (<= 1) cs
    (nl, nr) = span (> 1) r
    in filter (/= []) $ l : splitChesses nr



maxScore :: [PatternScore] -> Int
maxScore [] = 0
maxScore ns = maximum (map snd ns)

equalList :: (Eq a) => [a] -> [a] -> Bool
equalList x y = length x == length y && isInfixOf x y 

makeSymmetric :: [PatternScore] -> [PatternScore]
makeSymmetric pss = concat $ flip map pss $ \(ps, s) -> let
    rps = reverse ps
    in if equalList rps ps
       then [(ps, s)]
       else [(ps, s), (rps, s)]

createTree :: [PatternScore] -> PatternMatchTree
createTree [] = MissMatch
createTree pss = let
    (emptys, others) = partition firstEmpty pss
    (nones, allys) = partition isNone others
    firstEmpty (ps, s) = ps /= [] && head ps == Empty
    isNone ([], s) = True
    isNone (_, s) = False
    maxNone = maxScore nones
    tailPs (ps, s) = (tail ps, s) 
    cts = createTree . map tailPs
    in Node maxNone (cts emptys) (cts allys)
    
matchTreeCase = createTree symmetricPatternCases

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy com x y = if com x y == LT 
                then y
                else x

matchByTree :: PatternMatchTree -> [Chess] -> (Int, [Chess])
matchByTree MissMatch cs = (0, cs)
matchByTree node [] = (score node, [])
matchByTree node (c:cs) = let
    thisR = (score node, c:cs)
    ms f = let 
        r = matchByTree (f node) cs
        in maxBy (comparing fst) r thisR 
    in if c == 0
       then ms emptyNode
       else ms allyNode

match = matchByTree matchTreeCase

iterMatch :: (Int, [Chess]) -> (Int, [Chess])
iterMatch (scoreSum, cs) = let
    (nScoreSum, ncs) = match cs
    in (scoreSum + nScoreSum, ncs)

chessesJudgeWithoutSplit :: [Chess] -> Int
chessesJudgeWithoutSplit cs = fst $ fromJust $ find (null . snd) $ iterate iterMatch (0, cs) 
chessesJudgeSingle :: [Chess] -> Int
chessesJudgeSingle cs = let 
    css = splitChesses cs
    scores = map chessesJudgeWithoutSplit css
    in sum scores