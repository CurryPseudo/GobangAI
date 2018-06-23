module AI.State.Interal where

import Debug.Trace
import Text.Printf
import Data.Array
import Data.List

import qualified Data.Set as S

import AI.Pattern
import Gobang

fromChesses_ :: [Chess] -> Int
fromChesses_ [] = 0
fromChesses_ (c:cs) = 4 * fromChesses_ cs + c

fromState_ :: Int -> Int -> [Chess]
fromState_ 0 _ = []
fromState_ len state = let
    (ns, c) = state `divMod` 4
    in c : fromState_ (len - 1) ns

fromChesses :: [Chess] -> Int
fromChesses cs = fromChesses_ (reverse cs)  

fromState :: Int -> [Chess]
fromState state = reverse $ fromState_ 8 state

maxState :: Int
maxState = 4 ^ 8 - 1

minState :: Int
minState = 0

stateSequence :: [Int]
stateSequence = [minState..maxState]

stateTrans :: Int -> Chess -> Int -> Int
stateTrans pos c state = let
    divValue = (4 ^ (7 - pos))
    lc = mod (div state divValue) 4
    diff = c - lc
    debugFunc = trace (printf "divValue = %d, lc = %d, diff = %d" divValue lc diff)
    in state + diff * divValue

stateScores :: [Int]
stateScores = flip map stateSequence $ 
    chessesJudge . fromState

stateScoresArray :: Array Int Int
stateScoresArray = listArray (minState, maxState) $! stateScores

prepareStateScores :: IO ()
prepareStateScores = do 
    return $! stateScoresArray
    return ()

stateScore :: Int -> Int
stateScore = (!) stateScoresArray  

findScore :: Int -> Maybe Int
findScore score = let
    getTuple i = (i, stateScore i) 
    tuples = map getTuple stateSequence
    in do 
        (i_, s_) <- find (\(i, s) -> s == score) tuples
        return i_

scores :: S.Set Int
scores = S.fromList $ map stateScore stateSequence
