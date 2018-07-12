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
                       | MissMatch deriving (Show, Eq)

newtype PatternScore a = PatternScore ([Pattern], a) deriving (Show, Eq)


nonePatternScore :: (Ord a, Num a) => PatternScore a
nonePatternScore = PatternScore ([], 0)

psTuple :: PatternScore a -> ([Pattern], a)
psTuple (PatternScore tuple) = tuple

psPatterns :: PatternScore a -> [Pattern]
psPatterns = fst . psTuple

presentTree = let
    emptyNode = MissMatch
    allyNode = Node undefined MissMatch MissMatch
    in Node undefined emptyNode allyNode
    
present :: (PatternMatchTree a -> PatternMatchTree a) -> Int
present f = let
    n = f presentTree
    check MissMatch = 0
    check _ = 1
    in check n

psScore :: PatternScore a -> a
psScore = snd . psTuple

mapPatterns :: ([Pattern] -> [Pattern]) -> PatternScore a -> PatternScore a
mapPatterns f (PatternScore (ps, s)) = PatternScore (f ps, s)

mapScore :: (a -> a) -> PatternScore a -> PatternScore a
mapScore f (PatternScore (ps, s)) = PatternScore (ps, f s)
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

chessesJudge :: [Chess] -> ([[Int]], Int)
chessesJudge cs = let
    forward = chessesJudgeSingle cs
    backward = swapChessesJudgeSingle cs
    in foldPS $ forward ++ backward

swapChessesJudgeSingle :: (Num a, Ord a, Eq a) => [Chess] -> [PatternScore a]
swapChessesJudgeSingle cs = let
    ps = chessesJudgeSingle (map swapC cs)
    in map (mapScore negate . mapPatterns (map swapC)) ps

swapC :: Chess -> Chess
swapC 1 = 2
swapC 2 = 1
swapC x = x

foldPS :: (Num a) => [PatternScore a] -> ([[Int]], a)
foldPS pss = foldr step ([], 0) pss where
    step (PatternScore (ops, os)) (ps, s) = (ops:ps, s + os)

chessesJudgeSingle :: (Num a, Ord a, Eq a) => [Chess] -> [PatternScore a]
chessesJudgeSingle cs = let 
    css = splitChesses cs
    in filter ((/= 0) . psScore) $concatMap chessesJudgeWithoutSplit css

chessesJudgeWithoutSplit :: (Num a, Ord a, Eq a) => [Chess] -> [PatternScore a]
chessesJudgeWithoutSplit [] = []
chessesJudgeWithoutSplit cs = let
    f (pureP, pureS) = pureP:chessesJudgeWithoutSplit pureS
    rs = map f $ purePsPureSs cs
    in if null rs
       then []
       else maximumBy (comparing (sum . map psScore)) rs

purePsPureSs :: (Num a, Ord a, Eq a) => [Chess] -> [(PatternScore a, [Chess])]
purePsPureSs [] = []
purePsPureSs cs = let
    prefs = matchPrefix cs
    prefPs = map psPatterns prefs
    suffs = map (`removePrefixByEndZero` cs) prefPs
    in filter ((/= length cs) . length . snd) $ flip concatMap (zip prefs suffs) $ \(p, s) ->
        flip map s $ \s_ -> (p, s_)

removePrefixByEndZero :: [Pattern] -> [Pattern] -> [[Pattern]]
removePrefixByEndZero [] ps = [ps]
removePrefixByEndZero [0] ps = [ps, tail ps]
removePrefixByEndZero (prefP:prefPs) (p:ps) = removePrefixByEndZero prefPs ps






patternStrs :: (Num a, Eq a, Ord a) => [(String, a)]
patternStrs = 
    [ ("11111" , 1000)
    , ("011111" , 1000)
    , ("0111110" , 1000)
    , ("011110", 200)
    , ("01111" , 100)
    , ("010111" , 100)
    , ("011011" , 100)
    , ("011101" , 100)
    , ("0101110" , 100)
    , ("10111" , 100)
    , ("0110110" , 100)
    , ("11011" , 100)
    , ("01110" , 80) 
    , ("011010", 70)
    , ("0111"  , 40)
    , ("01101" , 35)
    , ("01011" , 34)
    , ("0110"  , 10)
    , ("01010" , 9)
    , ("011"   , 5)
    , ("0101"  , 4)
    ]
patternCases :: (Num a, Eq a, Ord a) => [PatternScore a]
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
    emptySub = subTree emptyNode
    allySub = subTree allyNode
    subTree f = append (present f) $ createTree (cutHead (belongs f))
    (emptyPs, otherPs) = partition (null . psPatterns) pss
    belongs f = flip filter otherPs $ \(PatternScore (ps, _)) -> 
        fromInteger (fromIntegral (head ps))  == present f
    cutHead = map (mapPatterns tail)
    append p MissMatch = MissMatch
    append p (Node (PatternScore (ps, s)) emn aln) = 
        Node (PatternScore (p:ps, s)) (append p emn) (append p aln)
    currentPs = fromMaybe nonePatternScore $ listToMaybe emptyPs
    in Node currentPs emptySub allySub
    
mapPMT :: (PatternScore a -> PatternScore a) -> PatternMatchTree a -> PatternMatchTree a
mapPMT _ MissMatch = MissMatch
mapPMT f (Node ps emptySub allySub) = Node (f ps) (mapPMT f emptySub) (mapPMT f allySub)

getAllPs :: PatternMatchTree a -> [PatternScore a]
getAllPs MissMatch = []
getAllPs (Node ps emptySub allySub) = [ps] ++ getAllPs emptySub ++ getAllPs allySub


matchTreeCase :: (Num a, Ord a, Eq a) => PatternMatchTree a
matchTreeCase = createTree patternCases 

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy com x y = if com x y == LT 
                then y
                else x

matchPrefixByTree :: PatternMatchTree a -> [Chess] -> [PatternScore a]
matchPrefixByTree MissMatch _ = []
matchPrefixByTree (Node ps _ _) [] = [ps]
matchPrefixByTree (Node ps eSub aSub) (c:cs) = let
    sub = if c == 0
          then eSub
          else aSub
    subR = matchPrefixByTree sub cs
    in ps:subR

scoreZero :: (Num a, Eq a) => PatternScore a -> Bool
scoreZero ps = psScore ps == 0




matchPrefix :: (Num a, Ord a, Eq a) => [Chess] -> [PatternScore a]
--matchPrefix c = filter (not . scoreZero) $ matchPrefixByTree matchTreeCase c
matchPrefix c = matchPrefixByTree matchTreeCase c

