module AI.StateSpec where

import AI.State
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "fromChesses" $ do
        it "return a quaternary number represent a state from 8 length chess sequence" $ do
            fromChesses [0,3,2,0,1,2,0,1] `shouldBe` 14433
            fromChesses [0,0,0,0,0,0,0,0] `shouldBe` 0
            fromChesses [1,1,1,1,1,1,1,1] `shouldBe` 21845
            fromChesses [1,1,1,1,0,1,1,1] `shouldBe` 21781
            fromChesses [3,3,3,3,3,3,3,3] `shouldBe` 65535
    describe "fromState" $ do 
        it "reverse function of fromChesses" $ do
            [0,3,2,0,1,2,0,1] `shouldBe` fromState 14433
            [0,0,0,0,0,0,0,0] `shouldBe` fromState 0
            [1,1,1,1,1,1,1,1] `shouldBe` fromState 21845
            [1,1,1,1,0,1,1,1] `shouldBe` fromState 21781
            [3,3,3,3,3,3,3,3] `shouldBe` fromState 65535
    describe "stateTrans" $ do
        it "return a state transition function from a chess position and a chess" $ do
            stateTrans 4 2 3124 `shouldBe` 3252
            stateTrans 2 0 14232 `shouldBe` 13208
            stateTrans 1 3 323 `shouldBe` 12611
    describe "stateScore" $ do
        it "return a score from a state" $ do
            stateScore (fromChesses [0,1,1,1,1,0,2,2]) `shouldBe` 194
            stateScore (fromChesses [0,2,2,2,0,0,2,2]) `shouldBe` -86
