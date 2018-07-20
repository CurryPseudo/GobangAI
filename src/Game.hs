module Game where

import AI.State.Board
import AI.Search

play :: BoardState -> ((Int, Int), Int) -> ((Int, Int), BoardState)
play bs (pos, chess) = (searched, updateTwice) where
    updateOnce = update bs (pos, chess)
    searched = search updateOnce (3 - chess) (6, 8, 1)
    updateTwice = update updateOnce (searched, 3 - chess)
    
playSelf :: (Int, Int) -> (Int, Int, Float) -> Int -> [((Int, Int), Int)]
playSelf size (m, n, c) stepCount = r where
    results = scanl step (empty size, 1, []) [0..stepCount - 1]
    notWin (bs, _, _) = abs (score bs) < 900000
    (_, _, r) 
        = if null $ dropWhile notWin results
          then last results 
          else head $ dropWhile notWin results
    step :: (BoardState, Int, [((Int, Int), Int)]) -> Int -> (BoardState, Int, [((Int, Int), Int)])
    step (origin, chess, last) _ = (nextState, nextChess, next) where
        nextPos = search origin chess (m, n, c)
        nextState = update origin (nextPos, chess)
        nextChess = 3 - chess
        next = last ++ [(nextPos, chess)]


