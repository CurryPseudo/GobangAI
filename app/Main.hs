module Main where

import AI.State
import Game
import Data.Array
import System.TimeIt

main :: IO ()
main = print (playSelf (19, 19) 10)

