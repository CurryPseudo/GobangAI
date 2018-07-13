module AI.StateSpec where

import AI.State
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "fromChesses" $ 
        it "return a quaternary number represent a state from 8 length chess sequence" $ do
            show (fromChesses [0,3,2,0,1,2,0,1]) `shouldBe` "[0,3,2,0,1,2,0,1]"
            show (fromChesses [0,0,0,0,0,0,0,0]) `shouldBe` "[0,0,0,0,0,0,0,0]"
            show (fromChesses [1,1,1,1,1,1,1,1]) `shouldBe` "[1,1,1,1,1,1,1,1]"
            show (fromChesses [1,1,1,1,0,1,1,1]) `shouldBe` "[1,1,1,1,0,1,1,1]"
            show (fromChesses [3,3,3,3,3,3,3,3]) `shouldBe` "[3,3,3,3,3,3,3,3]"
    describe "fromState" $  
        it "reverse function of fromChesses" $ do
            [0,3,2,0,1,2,0,1] `shouldBe` fromState (fromChesses [0,3,2,0,1,2,0,1])
            [0,0,0,0,0,0,0,0] `shouldBe` fromState (fromChesses [0,0,0,0,0,0,0,0])
            [1,1,1,1,1,1,1,1] `shouldBe` fromState (fromChesses [1,1,1,1,1,1,1,1])
            [1,1,1,1,0,1,1,1] `shouldBe` fromState (fromChesses [1,1,1,1,0,1,1,1])
            [3,3,3,3,3,3,3,3] `shouldBe` fromState (fromChesses [3,3,3,3,3,3,3,3])
    describe "stateTrans" $ 
        it "return a state transition function from a chess position and a chess" $ do
            stateTrans 4 2 (fromChesses [0,0,1,0,1,0,0,0]) `shouldBe` fromChesses [0,0,1,0,2,0,0,0]
            stateTrans 2 0 (fromChesses [1,2,2,1,2,1,2,1]) `shouldBe` fromChesses [1,2,0,1,2,1,2,1]
            stateTrans 1 1 (fromChesses [0,2,1,2,1,1,2,0]) `shouldBe` fromChesses [0,1,1,2,1,1,2,0]
    describe "stateScore" $ 
        it "return a score from a state and a chess center of state" $ do
            stateScore (fromChesses [0,1,1,1,1,0,2,2]) 1 `shouldBe` 9950 
            stateScore (fromChesses [0,1,1,1,1,0,2,2]) 2 `shouldBe` 350
            stateScore (fromChesses [0,2,2,2,0,0,2,2]) 1 `shouldBe` -450
            stateScore (fromChesses [0,2,2,2,0,0,2,2]) 2 `shouldBe` -2050
