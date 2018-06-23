module Main where

import AI.State
import Data.Array
import System.TimeIt

main :: IO ()
main = timeIt $ test $ do
    prepareStateScores
    return stateScore


test :: IO (Int -> Int) -> IO ()
test get = do
    ss <- get
    print (ss 800)
    print (ss 4800)
    print (ss 64800)