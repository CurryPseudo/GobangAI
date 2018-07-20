module Game where

import AI.State.Board
import AI.Search

play :: BoardState -> ((Int, Int), Int) -> ((Int, Int), BoardState)
play bs (pos, chess) = (searched, updateTwice) where
    updateOnce = update bs (pos, chess)
    searched = search updateOnce (3 - chess) (6, 8)
    updateTwice = update updateOnce (searched, 3 - chess)
    
playSelf :: (Int, Int) -> Int -> [((Int, Int), Int)]
playSelf size stepCount = r where
    results = scanl step (empty size, 1, []) [0..stepCount - 1]
    notWin (bs, _, _) = abs (score bs) < 900000
    (_, _, r) = last $ takeWhile notWin results
    step :: (BoardState, Int, [((Int, Int), Int)]) -> Int -> (BoardState, Int, [((Int, Int), Int)])
    step (origin, chess, last) _ = (nextState, nextChess, next) where
        nextPos = search origin chess (6, 15)
        nextState = update origin (nextPos, chess)
        nextChess = 3 - chess
        next = last ++ [(nextPos, chess)]


