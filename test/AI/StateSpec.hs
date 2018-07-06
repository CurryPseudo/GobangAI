module AI.StateSpec where

import AI.State
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "fromChesses" $ 
        it "return a quaternary number represent a state from 8 length chess sequence" $ do
            fromChesses [0,3,2,0,1,2,0,1] `shouldBe` 14433
            fromChesses [0,0,0,0,0,0,0,0] `shouldBe` 0
            fromChesses [1,1,1,1,1,1,1,1] `shouldBe` 21845
            fromChesses [1,1,1,1,0,1,1,1] `shouldBe` 21781
            fromChesses [3,3,3,3,3,3,3,3] `shouldBe` 65535
    describe "fromState" $  
        it "reverse function of fromChesses" $ do
            [0,3,2,0,1,2,0,1] `shouldBe` fromState 14433
            [0,0,0,0,0,0,0,0] `shouldBe` fromState 0
            [1,1,1,1,1,1,1,1] `shouldBe` fromState 21845
            [1,1,1,1,0,1,1,1] `shouldBe` fromState 21781
            [3,3,3,3,3,3,3,3] `shouldBe` fromState 65535
    describe "stateTrans" $ 
        it "return a state transition function from a chess position and a chess" $ do
            stateTrans 4 2 3124 `shouldBe` 3252
            stateTrans 2 0 14232 `shouldBe` 13208
            stateTrans 1 3 323 `shouldBe` 12611
    describe "stateScore" $ 
        it "return a score from a state and a chess center of state" $ do
            stateScore (fromChesses [0,1,1,1,1,0,2,2]) 1 `shouldBe` 994 
            stateScore (fromChesses [0,1,1,1,1,0,2,2]) 2 `shouldBe` 34
            stateScore (fromChesses [0,2,2,2,0,0,2,2]) 1 `shouldBe` -46
            stateScore (fromChesses [0,2,2,2,0,0,2,2]) 2 `shouldBe` -206
