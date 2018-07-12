module AI.State.Internal where

import Debug.Trace
import Text.Printf
import Data.Array
import Data.List

import qualified Data.Set as S

import AI.Pattern
import Gobang

newtype State = St Int deriving(Eq)

toInt :: State -> Int
toInt (St x) = x

instance Show State where
    show s = show (fromState s)

fromChesses_ :: [Chess] -> State
fromChesses_ [] = St 0
fromChesses_ (c:cs) = St $ 4 * toInt (fromChesses_ cs) + c

fromState_ :: Int -> State -> [Chess]
fromState_ 0 _ = []
fromState_ len state = let
    (ns, c) = toInt state `divMod` 4
    in c : fromState_ (len - 1) (St ns)

fromChesses :: [Chess] -> State
fromChesses cs = fromChesses_ (reverse cs)  

fromState :: State -> [Chess]
fromState state = reverse $ fromState_ 8 state

maxState :: Int
maxState = 4 ^ 8 - 1

minState :: Int
minState = 0

stateSequence :: [Int]
stateSequence = [minState..maxState]

stateTrans :: Int -> Chess -> State -> State
stateTrans pos c state = let
    divValue = (4 ^ (7 - pos))
    lc = mod (div (toInt state) divValue) 4
    diff = c - lc
    in St $ toInt state + diff * divValue

stateScores :: [[Int]]
stateScores = flip map stateSequence $ \s ->
    map (placeCenterScore (fromState (St s))) [none, ally, enemy]

placeCenterScore :: [Chess] -> Chess -> Int
placeCenterScore cs c = let
    half = length cs `quot` 2
    (left, right) = splitAt half cs
    nc = left ++ [c] ++ right
    (ps, s) = chessesJudge nc
    in s


stateScoresArray :: Array Int [Int]
stateScoresArray = listArray (minState, maxState) $! stateScores

chessToScore :: [Int] -> Chess -> Int
chessToScore ss c = ss !! c

prepareStateScores :: IO ()
prepareStateScores = do 
    return $! stateScoresArray
    return ()


stateScore :: State -> Chess -> Int
stateScore (St state) = chessToScore (stateScoresArray ! state) 

