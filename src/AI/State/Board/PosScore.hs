module AI.State.Board.PosScore where

import Text.Printf

import AI.State.Board.Vector

data PosScore = PS {pos :: Pos, scores :: Pos -> [Int], chess :: Int}

toScores :: PosScore -> Int
toScores ps = (!! chess ps) (ps `scores` pos ps)

instance Eq PosScore where
    ps1 == ps2 = pos ps1 == pos ps2

instance Ord PosScore where
    ps1 `compare` ps2 = comp (chess ps1) (toScores ps1) (toScores ps2) where
        comp 0 _ _ = EQ
        comp 1 x y = compare y x
        comp 2 x y = compare x y

instance Show PosScore where
    show ps = printf "(Pos: %s, Chess: %s, Score: %s)" (show (pos ps)) (show (toScores ps)) (show (chess ps))

updateSortedList :: PosScore -> [PosScore] -> [PosScore]
updateSortedList ps pss = let
    find :: Bool
    find = 
