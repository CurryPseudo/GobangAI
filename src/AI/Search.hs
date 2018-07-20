module AI.Search where

import Data.List
import Data.Maybe
import Debug.Trace
import Control.Exception.Base

import Board
import AI.State.Board
import AI.State.Board.Vector

search :: BoardState -> Int -> (Int, Int, Float) -> Pos
search boardState chessOrigin (originM, originN, decreaseCoefficient) = resultPos where
    all = searchSub boardState chessOrigin (Nothing, Nothing) originM
    (_, (resultPos, _):_) = all
    searchSub :: BoardState -> Int -> (Maybe Int, Maybe Int) -> Int -> (Int, [(Pos, Int)])
    searchSub bs _ _ 0 = (score bs, [])
    searchSub boardState chess (alpha, beta) m = r where
        searchedPoses = chessBest boardState chess (max (floor (realToFrac originN - (realToFrac (originM - m) * decreaseCoefficient))) 1)
        scanResult = scanl step ((alpha, beta), Nothing, False) $ assert (not (null searchedPoses)) searchedPoses
        stop (_, _, x) = not x
        stoped = dropWhile stop scanResult
        (_, Just r_, _) 
            = if null stoped
              then last scanResult
              else head stoped
        r = if abs (score boardState) > 900000
            then (score boardState, [])
            else r_
        step :: ((Maybe Int, Maybe Int), Maybe (Int, [(Pos, Int)]), Bool) -> Pos -> ((Maybe Int, Maybe Int), Maybe (Int, [(Pos, Int)]), Bool)
        step ((alpha, beta), origin, _) pos = result where
            subBs :: BoardState
            subBs = update boardState (pos, chess)
            subScore :: Int
            subPosElems :: [(Pos, Int)]
            (subScore, subPosElems) = searchSub subBs (3 - chess) (alpha, beta) (m - 1)
            stopSearch :: Bool
            stopSearch = isJust alpha && isJust beta && fromJust alpha > fromJust beta
            updateAlphaBeta :: Int -> ((Maybe Int, Maybe Int), Bool)
            updateAlphaBeta 1 
                = if isNothing alpha || subScore > fromJust alpha
                  then ((Just subScore, beta), True)
                  else ((alpha, beta), False)
            updateAlphaBeta 2
                = if isNothing beta || subScore < fromJust beta
                  then ((alpha, Just subScore), True)
                  else ((alpha, beta), False)
            (alphaBeta, updated) = updateAlphaBeta chess
            resultPosElems = (pos, chess):subPosElems
            replaced = Just (subScore,resultPosElems)
            result :: ((Maybe Int, Maybe Int), Maybe (Int, [(Pos, Int)]), Bool)
            result = (alphaBeta, if updated then replaced else if isJust origin then origin else replaced, stopSearch)
            traceFunc = trace $ show (subScore, subPosElems, stopSearch, alphaBeta, chess)

bugCase :: BoardState
bugCase = fromBoard $ fromJust $ listBoard $ 
    reverse [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] --18
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] --17
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] --16
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] --15
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] --14
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] --13
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] --12
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] --11
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] --10
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 9
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 8
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 7
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 6
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 5
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 4
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 3
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 2
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 1
            , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] -- 0
--             0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
            ]

bugInputs = [(9,4), (8,10)]
bugInputs2 = [(8,6)]
