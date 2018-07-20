module Main where

import Data.Array
import System.Environment

import AI.State
import Game

import System.TimeIt

main :: IO ()
main = do
    putStrLn "Press a board size"
    size <- getLine
    putStrLn "Press m, n and c"
    mn <- getLine
    putStrLn "Press cac max step count"
    stepCount <- getLine
    timeIt $ print (playSelf (read size) (read mn) (read stepCount))

