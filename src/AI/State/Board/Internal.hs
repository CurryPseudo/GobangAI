module AI.State.Board.Internal where

import Data.Ord
import Data.Tuple
import Data.Maybe
import Data.List
import Control.Monad

import qualified Data.Map.Strict as M
import Data.Array

import Data.Array.Utils
import Board
import AI.State

data BoardState = BS {score :: !Int, states :: !ArrayState, bestChoice :: ![(Pos, Pos)]} deriving (Eq, Show)
type ArrayState = Array (Int, Int) DirStates 
data Dir = Horizontal | Vertical | MainDiag | Diag deriving (Show, Eq, Ord)
type DirStates = (Int, M.Map Dir State, (Int, Int))
type Pos = (Int, Int)
type Size = (Int, Int)

empty :: (Int, Int) -> Int -> BoardState
empty (w, h) bestCount = BS 0 as choice where
    as = genTableArray (w, h) $ buildDirStates (emptyBoard w h)
    ie = assocs as
    ns f (_, _, s) = f s 
    sorted f = map fst $ sortBy (comparing (ns f . snd)) ie
    choice = zip (take bestCount (sorted fst)) (take bestCount (reverse (sorted snd)))

fromBoard :: Board -> Int -> BoardState
fromBoard b bestCount = let
    updatePos pos bs = update bs (pos, getElem b pos)
    size = getSize b
    in foldr updatePos (empty size bestCount) $ getIndexes size

buildDirStates :: Board -> Pos -> DirStates
buildDirStates b pos = let
    dirState dir = fromChesses $ map (getElem b) $ dirSeq pos dir
    insertDir (d, vec) = M.insert d (dirState vec)
    rMap = foldr insertDir M.empty dirVecs
    re = getElem b pos
    cScore c = dirStatesTransScore re rMap c + locationScore (getSize b) pos * chessRltSign re c
    in (re, rMap, (cScore 1, cScore 2))

dirStatesTransScore :: Int -> M.Map Dir State -> Int -> Int
dirStatesTransScore ce m e = let
    deltaStateScore state = stateScore state e - stateScore state ce
    deltaScores = map deltaStateScore (M.elems m) 
    in sum deltaScores

update :: BoardState -> PosElem -> BoardState
update bs@(BS sc sts choices) pe@(p, e) = let
    (ce, baseDirStates, (best, worst)) = sts ! p 
    score = dirStatesTransScore ce baseDirStates e + locationScore (getSize sts) p * chessRltSign ce e
    (w, h) = getSize sts
    ndUdtPoses = udtedStatesPoses w h p
    udtAsFuncs = flip map ndUdtPoses $ \ pos -> mapAsUdtStates pos (udtStatesBase pos pe)
    nsts = foldr (\f x -> f x) sts udtAsFuncs // [(p, (e, baseDirStates))]
    in BS (sc + score) nsts

udtedStatesPoses :: Int -> Int -> Pos -> [Pos]
udtedStatesPoses w h p@(x, y) = let
    between min max x = x >= min && x <= max
    inEdge (x, y) = between 0 (w - 1) x && between 0 (h - 1) y
    in filter inEdge $ udtedStatesPosesUnFilter p

locationScore :: Size -> Pos -> Int
locationScore (w, h) (x, y) = let
    score m n = max m (n - 1 - m)
    in score x w + score y h

chessRltSign :: Int -> Int -> Int
chessRltSign 0 x = (x - 1) * (-2) + 1
chessRltSign x 0 = - chessRltSign 0 x
chessRltSign x y = (x - y) * 2

udtedStatesPosesUnFilter :: Pos -> [Pos]
udtedStatesPosesUnFilter p = flip concatMap dirVecs $ \(_, d) ->
    dirSeq p d

udtStatesBase :: Pos -> PosElem -> DirStates -> DirStates
udtStatesBase pos (bp, be) ds = let
    Just (dir, statePos) = rltvDirPos pos bp
    fds :: M.Map Dir State -> M.Map Dir State
    fds = M.adjust (stateTrans statePos be) dir
    in mapSnd fds ds

mapAsUdtStates :: Pos -> (DirStates -> DirStates) -> ArrayState -> ArrayState
mapAsUdtStates pos f as = as // [(pos, f (as ! pos))]

mapSnd :: (b -> b) -> (a, b) -> (a, b)
mapSnd f (x, y) = (x, f y)

doPos2 :: (Int -> Int -> Int) -> Pos -> Pos -> Pos
doPos2 f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

doPos :: (Int -> Int) -> Pos -> Pos
doPos f (x, y) = (f x, f y)

pAdd = doPos2 (+)
pSub = doPos2 (-)
pMul = doPos2 (*)
pQuot = doPos2 quot
rltvDirPos :: Pos -> Pos -> Maybe (Dir, Int)
rltvDirPos (bx, by) (x, y) = let
    d = (x - bx, y - by)
    in msum $ flip map dirVecs $ \(dir, vec) -> do
        len <- vecLen d vec
        let pos = if len > 0
                  then len + 3
                  else len + 4
        return (dir, pos)

dirVecs :: [(Dir, (Int, Int))]
dirVecs = [ (Horizontal, (1, 0))
          , (Vertical, (0, 1))
          , (MainDiag, (1, 1))
          , (Diag, (1, -1))
          ]

vecLen :: (Int, Int) -> (Int, Int) -> Maybe Int
vecLen (0, 0) _ = Nothing
vecLen (dx, dy) (rx, ry) = if dx * ry == dy * rx
                           then if rx /= 0
                                then Just $ dx `quot` rx
                                else Just $ dy `quot` ry
                           else Nothing

stateSeq = [-4, -3, -2, -1, 1, 2, 3, 4]

dirSeq :: Pos -> (Int, Int) -> [Pos]
dirSeq p (rx, ry)= flip map stateSeq $ \l ->
    pAdd p (l * rx, l * ry)
