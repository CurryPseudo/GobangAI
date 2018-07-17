module AI.State.Board.Internal where

import Data.Ord
import Data.Tuple
import Data.Maybe
import Data.List
import Text.Printf
import Debug.Trace

import qualified Data.Map.Strict as M
import Data.Array

import Data.Array.Utils
import Board
import AI.State
import AI.State.Board.Vector
import AI.State.Board.Dir
import AI.State.Board.PosScore

data BoardState = BS {score :: !Int, states :: !ArrayState, bestChoice :: ![[PosScore]]} deriving (Eq, Show)
type ArrayState = Array (Int, Int) DirStates 
type DirStates = (Int, M.Map Dir State, [Int])

getBoardElem :: DirStates -> Int
getBoardElem (e, _, _) = e

toDirStateMap :: DirStates -> M.Map Dir State
toDirStateMap (_, dsm, _) = dsm

toScoresMap :: DirStates -> [Int]
toScoresMap (_, _, sm) = sm


empty :: (Int, Int) -> Int -> BoardState
empty (w, h) bestCount = BS 0 as choice where
    as = genTableArray (w, h) $ buildDirStates (emptyBoard w h)
    ie = assocs as
    sorted f = map fst $ sortOn (f . toScoresMap . snd) ie
    choice = map (sort . (\chess -> map (\pos -> genPosScore as pos chess) (getIndexes (w, h)))) [0..2]

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
    in (re, rMap, deltaScoreCombine rMap (getSize b) pos re)

dirStatesTransScore :: Int -> M.Map Dir State -> Int -> Int
dirStatesTransScore ce m e = let
    deltaStateScore state = stateScore state e - stateScore state ce
    deltaScores = map deltaStateScore (M.elems m) 
    in sum deltaScores

deltaScoreCombine :: M.Map Dir State -> Size -> Pos ->  Int -> [Int]
deltaScoreCombine dsm s pos ce = flip map [0..2] $ \c -> dirStatesTransScore ce dsm c + locationScore s pos * chessRltSign ce c


update :: BoardState -> PosElem -> BoardState
update bs@(BS sc sts choices) pe@(p, e) = let
    (ce, baseDirStates, scoreMap) = sts ! p 
    deltaScore = scoreMap !! e
    (w, h) = getSize sts
    ndUdtPoses = aroundDirPosesInEdge (w, h) p
    scoreTrans p (e, dsm, _) = (e, dsm, deltaScoreCombine dsm (w, h) p e)
    udtAsFuncs = flip map ndUdtPoses $ \ pos -> mapAsUdtStates pos (scoreTrans pos . udtStatesBase pos pe)
    nsts = foldr (\f x -> f x) sts udtAsFuncs // [(p, (e, baseDirStates, deltaScoreCombine baseDirStates (w, h) p e))]
    emptyPoses = filter (\pos -> getBoardElem (sts ! pos) == 0) ndUdtPoses
    updateChoices :: Pos -> [[PosScore]] -> [[PosScore]]
    updateChoices pos = zipWith updateSub [0..2] where
        updateSub :: Int -> [PosScore] -> [PosScore]
        updateSub chess = undefined
    nChoices = foldr updateChoices choices emptyPoses
    in BS(sc + deltaScore) nsts nChoices

locationScore :: Size -> Pos -> Int
locationScore (w, h) (x, y) = let
    score m n = min m (n - 1 - m)
    in score x w + score y h

chessRltSign :: Int -> Int -> Int
chessRltSign 0 0 = 0
chessRltSign 0 x = (x - 1) * (-2) + 1
chessRltSign x 0 = - chessRltSign 0 x
chessRltSign x y = (x - y) * 2


udtStatesBase :: Pos -> PosElem -> DirStates -> DirStates
udtStatesBase pos (bp, be) (e, m, scoreMap) = let
    Just (dir, statePos) = relativeDirLength pos bp
    fds :: M.Map Dir State -> M.Map Dir State
    fds = M.adjust (stateTrans statePos be) dir
    in (e, fds m, scoreMap)

mapAsUdtStates :: Pos -> (DirStates -> DirStates) -> ArrayState -> ArrayState
mapAsUdtStates pos f as = as // [(pos, f (as ! pos))]



testBoardState :: BoardState
testBoardState = flip fromBoard 10 $ fromJust $ listBoard [ [0,0,0,0,0,0,0,0,0]
                                                          , [0,0,0,0,0,0,0,0,0]
                                                          , [0,0,0,0,0,0,0,0,0]
                                                          , [0,0,0,0,0,2,1,0,0]
                                                          , [0,0,0,1,1,1,2,0,0]
                                                          , [0,0,0,0,0,2,2,0,0]
                                                          , [0,0,0,0,0,0,0,0,0]
                                                          , [0,0,0,0,0,0,0,0,0]
                                                          ]                    

genPosScore :: ArrayState -> Pos -> Int -> PosScore
genPosScore as pos = PS pos scoresFunc where
    scoresFunc :: Pos -> [Int]
    scoresFunc = toScoresMap . (as !)