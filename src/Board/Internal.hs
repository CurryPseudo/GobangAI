module Board.Internal where

import Data.Array
import Data.Either
import Data.Maybe
import Data.Array.Utils

type Board = Array (Int, Int) Int

type PosElem = ((Int, Int), Int)

putElem :: Board -> PosElem -> Maybe Board
putElem a pe@(_, e) = if inElemRange e
                      then Just (a // [pe])
                      else Nothing

putElems :: Board -> [PosElem] -> Maybe Board
putElems a [] = Just a
putElems a (pe:pes) = do
    na <- putElem a pe
    putElems na pes

emptyBoard :: Int -> Int -> Board
emptyBoard w h = genTableArray w h (const 0) 

listBoard :: [[Int]] -> Maybe Board
listBoard ls = let
    w = length $ head ls
    h = length ls
    f (x, y) = (ls !! y) !! x
    in if equalW ls && all (all inElemRange) ls
        then Just (genTableArray w h f)
        else Nothing

inElemRange :: Int -> Bool
inElemRange x = x <= 2 && x >= 0

equalW :: [[Int]] -> Bool
equalW [] = True
equalW (l:ls) = let
    w = length l
    f l = length l == w
    in all f ls

size :: Board -> (Int, Int)
size b = let
    (_, (w', h')) = bounds b
    in (w' + 1, h' + 1)

toLists :: Board -> [[Int]]
toLists b = let 
    (w, h) = size b
    indexes = getIndexes w h
    list = map (b !) indexes
    in chunksOf list w

chunksOf :: [a] -> Int -> [[a]]
chunksOf l w = if length l <= w
               then [l]
               else take w l : chunksOf (drop w l) w
