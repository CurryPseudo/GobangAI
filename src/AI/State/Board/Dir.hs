module AI.State.Board.Dir where

import Control.Monad

import AI.State.Board.Vector

data Dir = Horizontal | Vertical | MainDiag | Diag deriving (Show, Eq, Ord)


dirVecs :: [(Dir, Pos)]
dirVecs = [ (Horizontal, (1, 0))
          , (Vertical, (0, 1))
          , (MainDiag, (1, 1))
          , (Diag, (1, -1))
          ]

stateSeq = [-4, -3, -2, -1, 1, 2, 3, 4]

dirSeq :: Pos -> (Int, Int) -> [Pos]
dirSeq p (rx, ry)= flip map stateSeq $ \l ->
    pAdd p (l * rx, l * ry)

relativeDirLength :: Pos -> Pos -> Maybe (Dir, Int)
relativeDirLength (bx, by) (x, y) = let
    d = (x - bx, y - by)
    in msum $ flip map dirVecs $ \(dir, vec) -> do
        len <- vecLen d vec
        let pos = if len > 0
                  then len + 3
                  else len + 4
        return (dir, pos)

aroundDirPosesInEdge :: Size -> Pos -> [Pos]
aroundDirPosesInEdge (w, h) p@(x, y) = let
    between min max x = x >= min && x <= max
    inEdge (x, y) = between 0 (w - 1) x && between 0 (h - 1) y
    unFiltered :: [Pos]
    unFiltered = flip concatMap dirVecs $ \(_, d) ->
        dirSeq p d
    in filter inEdge unFiltered
 