module AI.State.Board.Vector where

type Pos = (Int, Int)
type Size = (Int, Int)

doPos2 :: (Int -> Int -> Int) -> Pos -> Pos -> Pos
doPos2 f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

doPos :: (Int -> Int) -> Pos -> Pos
doPos f (x, y) = (f x, f y)

pAdd :: Pos -> Pos -> Pos
pAdd = doPos2 (+)

pSub :: Pos -> Pos -> Pos
pSub = doPos2 (-)

pMul :: Pos -> Pos -> Pos
pMul = doPos2 (*)

pQuot :: Pos -> Pos -> Pos
pQuot = doPos2 quot

vecLen :: (Int, Int) -> (Int, Int) -> Maybe Int
vecLen (0, 0) _ = Nothing
vecLen (dx, dy) (rx, ry) = if dx * ry == dy * rx
                           then if rx /= 0
                                then Just $ dx `quot` rx
                                else Just $ dy `quot` ry
                           else Nothing