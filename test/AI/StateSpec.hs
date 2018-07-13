module AI.StateSpec where

import AI.State
import AI.Pattern
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "fromChesses" $
        it "return a quaternary number represent a state from 8 length chess sequence" $ property $ 
            forAll (vectorOf 8 (choose (0, 3))) $ \xs -> show (fromChesses xs) == show xs
    describe "fromState" $  
        it "reverse function of fromChesses" $ property $
            forAll (vectorOf 8 (choose (0, 3))) $ \xs -> xs == fromState (fromChesses xs)
    describe "stateTrans" $ 
        it "return a state transition function from a chess position and a chess" $ property $
            let 
                generator = do
                    pos <- choose (0, 7)
                    chess <- choose (0, 3)
                    xs <- vectorOf 8 (choose (0, 3))
                    return (pos, chess, xs)
            in forAll generator $ \(pos, chess, xs) ->
                let
                    r = fromState (stateTrans pos chess (fromChesses xs))
                    in r !! pos == chess && all (\ x -> x == pos || r !! x == xs !! x) [0 .. 7]
    describe "stateScore" $ 
        it "return a score from a state and a chess center of state" $ property $
            let
                generator = do
                    xs <- vectorOf 8 (choose (0, 3))
                    chess <- choose (0, 2)
                    return (xs, chess)
            in forAll generator $ \(xs, chess) ->
                stateScore (fromChesses xs) chess == snd (chessesJudge (take 4 xs ++ [chess] ++ drop 4 xs))
