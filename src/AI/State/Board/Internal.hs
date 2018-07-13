module AI.State.Board.Internal where

import Control.Monad


import qualified Data.Map.Strict as M
import Data.Array

import Data.Array.Utils
import Data.Maybe
import Board
import AI.State

data BoardState = BS {score :: Int, states :: ArrayState} deriving (Eq, Show)
type ArrayState = Array (Int, Int) DirStates 
data Dir = Horizontal | Vertical | MainDiag | Diag deriving (Show, Eq, Ord)
type DirStates = (Int, M.Map Dir State)
type Pos = (Int, Int)
type Size = (Int, Int)

empty :: (Int, Int) -> BoardState
empty (w, h) = BS 0 (genTableArray (w, h) $ buildDirStates (emptyBoard w h))


fromBoard :: Board -> BoardState
fromBoard b = let
    updatePos pos bs = update bs (pos, getElem b pos)
    size = getSize b
    in foldr updatePos (empty size) $ getIndexes size

buildDirStates :: Board -> Pos -> DirStates
buildDirStates b pos = let
    dirState dir = fromChesses $ map (getElem b) $ dirSeq pos dir
    insertDir (d, vec) = M.insert d (dirState vec)
    rMap = foldr insertDir M.empty dirVecs
    re = getElem b pos
    in (re, rMap)

update :: BoardState -> PosElem -> BoardState
update bs@(BS sc sts) pe@(p, e) = let
    (ce, baseDirStates) = sts ! p 
    deltaStateScore state = stateScore state e - stateScore state ce
    deltaScores = map deltaStateScore (M.elems baseDirStates) 
    deltaScore = sum deltaScores
    (w, h) = getSize sts
    ndUdtPoses = udtedStatesPoses w h p
    udtAsFuncs = flip map ndUdtPoses $ \ pos -> mapAsUdtStates pos (udtStatesBase pos pe)
    nsts = foldr (\f x -> f x) sts udtAsFuncs // [(p, (e, baseDirStates))]
    in BS (sc + fromInteger (toInteger deltaScore)) nsts

udtedStatesPoses :: Int -> Int -> Pos -> [Pos]
udtedStatesPoses w h p@(x, y) = let
    between min max x = x >= min && x <= max
    inEdge (x, y) = between 0 (w - 1) x && between 0 (h - 1) y
    in filter inEdge $ udtedStatesPosesUnFilter p

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
