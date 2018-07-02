module BoardSpec where

import Control.Exception

import Test.Hspec

import Board

testBoard2 = putElem testBoard ((0,0), 2)
testBoard3 = putElems testBoard [((0,2), 1), ((1,1), 0)]



spec :: Spec
spec = do
    describe "emptyBoard" $ do
        it "should return a empty board" $ do
            toLists (emptyBoard 4 4) `shouldBe` [ [0,0,0,0]
                                                , [0,0,0,0]
                                                , [0,0,0,0]
                                                , [0,0,0,0]
                                                ]
            toLists (emptyBoard 3 3) `shouldBe` [ [0,0,0]
                                                , [0,0,0]
                                                , [0,0,0]
                                                ]
    describe "listBoard" $ do
        it "should return a board constructed by a list" $ do
            toLists (listBoard [ [0,0,0,0] ]) `shouldBe` anyException
    
