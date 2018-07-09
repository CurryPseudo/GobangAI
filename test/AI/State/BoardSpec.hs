module AI.State.BoardSpec where

import Data.Maybe

import Test.Hspec
    
import AI.State.Board
import Board

spec :: Spec
spec = 
    describe "fromBoard" $
        it "should return a board state and score from a board" $ do
            let sb = score . fromBoard . fromJust
            let
                b1 = listBoard [ [0, 0, 0, 0, 0]
                               , [0, 1, 1, 0, 0]
                               , [0, 0, 0, 0, 0]
                               ]
                b2 = listBoard [ [0, 0, 0, 0, 0]
                               , [0, 0, 1, 0, 0]
                               , [0, 0, 1, 0, 0]
                               , [0, 0, 0, 0, 0]
                               ]   
                in compare (sb b1) (sb b2) `shouldBe` EQ
            
            let 
                b1 = listBoard [ [0, 0, 0, 0, 0]
                               , [0, 2, 0, 0, 0]
                               , [0, 1, 1, 0, 0]
                               , [0, 1, 0, 1, 0]
                               , [0, 0, 0, 0, 0]
                               ]
                b2 = listBoard [ [0, 0, 0, 0, 0]
                               , [0, 0, 0, 0, 0]
                               , [0, 1, 1, 0, 0]
                               , [0, 1, 2, 1, 0]
                               , [0, 0, 0, 0, 0]
                               ]
                in compare (sb b1) (sb b2) `shouldBe` LT
            let 
                b1 = listBoard [ [0, 0, 0, 0, 0, 0, 0]
                               , [0, 1, 2, 0, 0, 0, 0]
                               , [0, 1, 0, 2, 0, 0, 0]
                               , [0, 1, 0, 0, 2, 0, 0]
                               , [0, 1, 0, 0, 0, 2, 0]
                               , [0, 1, 0, 0, 0, 0, 0]
                               ]
                b2 = listBoard [ [0, 0, 0, 0, 0, 0, 0]
                               , [0, 1, 2, 0, 0, 0, 0]
                               , [0, 1, 0, 2, 0, 0, 0]
                               , [0, 1, 0, 0, 2, 0, 0]
                               , [0, 1, 0, 0, 0, 0, 0]
                               , [0, 0, 0, 0, 0, 0, 0]
                               ]
                in compare (sb b1) (sb b2) `shouldBe` GT

        

                

