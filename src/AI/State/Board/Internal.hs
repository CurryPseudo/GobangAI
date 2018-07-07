{-# LANGUAGE ExistentialQuantification #-}
module AI.State.Board.Internal where

import Data.Array
import Data.Array.Utils
import Board
data BoardState = forall a. (Num a) => BS a (Array (Int, Int) [DirState])

empty :: Int -> Int -> BoardState
empty w h = BS 0 (genTableArray w h $ const 0)

fromBoard :: Board -> BoardState
fromBoard b = undefined

update :: BoardState -> PosElem -> BoardState
update bs pe = let
    BS s a = bs

    


getScore :: (Num a) => BoardState -> a
getScore BS sc _ = sc

getState :: BoardState -> Array (Int, Int) Int
getState BS _ st = st