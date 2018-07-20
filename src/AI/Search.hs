module AI.Search where

import Data.Maybe
import Debug.Trace

import AI.State.Board
import AI.State.Board.Vector

search :: BoardState -> Int -> (Int, Int) -> Pos
search boardState chessOrigin (m, n) = resultPos where
    all = searchSub boardState chessOrigin (Nothing, Nothing) m
    (_, (resultPos, _):_) = all
    searchSub :: BoardState -> Int -> (Maybe Int, Maybe Int) -> Int -> (Int, [(Pos, Int)])
    searchSub bs _ _ 0 = (score bs, [])
    searchSub boardState chess (alpha, beta) m = r where
        (_, Just r) = foldr step ((alpha, beta), Nothing) (chessBest boardState chess n)
        step :: Pos -> ((Maybe Int, Maybe Int), Maybe (Int, [(Pos, Int)])) -> ((Maybe Int, Maybe Int), Maybe (Int, [(Pos, Int)]))
        step pos ((alpha, beta), origin) = result where
            subBs :: BoardState
            subBs = update boardState (pos, chess)
            subScore :: Int
            subPosElems :: [(Pos, Int)]
            (subScore, subPosElems) = searchSub subBs (3 - chess) (alpha, beta) (m - chess + 1)
            stopSearch :: Bool
            stopSearch = isJust alpha && isJust beta && fromJust alpha > fromJust beta
            updateAlphaBeta :: Int -> ((Maybe Int, Maybe Int), Bool)
            updateAlphaBeta 1 
                = if isJust alpha && subScore > fromJust alpha
                  then ((Just subScore, beta), True)
                  else ((alpha, beta), False)
            updateAlphaBeta 2
                = if isJust beta && subScore < fromJust beta
                  then ((alpha, Just subScore), True)
                  else ((alpha, beta), False)
            (alphaBeta, updated) = updateAlphaBeta chess
            resultPosElems = (pos, chess):subPosElems
            replaced = Just (subScore,resultPosElems)
            result :: ((Maybe Int, Maybe Int), Maybe (Int, [(Pos, Int)]))
            result = (alphaBeta, if updated then replaced else if isJust origin then origin else replaced)


