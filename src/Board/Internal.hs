module Board.Internal where

import Data.Array
import Data.Maybe

type Board = Array (Int, Int) Int

type PosElem = ((Int, Int), Int)

putElem :: Board -> PosElem -> Board
putElem a pe = a // [pe]

putElems :: Board -> [PosElem] -> Board
putElems = (//)

emptyBoard :: Int -> Int -> Board
emptyBoard w h = listBoard' w h $ replicate (w * h) 0 

listBoard_ :: [[Int]] -> Maybe Board
listBoard_ ls = let
    h = length $ head ls
    w = length ls
    in if equalW ls
        then Just $ listBoard' w h $ concat ls
        else Nothing

listBoard = fromJust . listBoard_

equalW :: [[Int]] -> Bool
equalW [] = True
equalW (l:ls) = let
    w = length l
    f l = length l == w
    in all f ls

listBoard' :: Int -> Int -> [Int] -> Board
listBoard' w h = listArray ((0, 0), (w - 1, h - 1))

size :: Board -> (Int, Int)
size b = let
    (_, (w', h')) = bounds b
    in (w' + 1, h' + 1)

toLists :: Board -> [[Int]]
toLists b = let 
    (w, _) = size b
    list = elems b
    in chunksOf list w

chunksOf :: [a] -> Int -> [[a]]
chunksOf l w = if length l <= w
               then [l]
               else take w l : chunksOf (drop w l) w



