module BoardSpec where
import Test.Hspec
import Board


spec :: Spec
spec = do
    describe "emptyBoard" $ 
        it "create a proper size of board" $ do
            emptyBoard 3 4 `shouldBe` emptyBoard 3 4
            emptyBoard 3 3 `shouldNotBe` emptyBoard 3 4
    describe "toLists" $ 
        it "should return a lists present a board" $ 
            toLists (emptyBoard 4 3) `shouldBe` [ [0, 0, 0, 0]
                                              , [0, 0, 0, 0]
                                              , [0, 0, 0, 0]
                                              ]
    describe "listBoard" $ 
        it "should return a board from lists" $ do
            emptyBoard 3 4 `shouldBe` listBoard [ [0, 0, 0]
                                                , [0, 0, 0]
                                                , [0, 0, 0]
                                                , [0, 0, 0]
                                                ]
            
            
    describe "putElems" $  
        it "should return a puted board from elems and a origin board" $ do
            let origin = listBoard [ [1, 0, 0]
                                   , [0, 0, 2]
                                   , [0, 1, 0]
                                   , [0, 0, 0]
                                   ] 

            let puted = putElems origin [ ((0, 0), 0)
                                        , ((2, 1), 1)
                                        , ((0, 1), 2)
                                        ] 
            puted `shouldBe` listBoard [ [0, 0, 0]
                                       , [2, 0, 1]  
                                       , [0, 1, 0]  
                                       , [0, 0, 0]  
                                       ]
    
                                    

