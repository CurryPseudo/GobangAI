module BoardSpec where
import Data.Maybe
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
            Just (emptyBoard 3 4) `shouldBe` listBoard [ [0, 0, 0]
                                                       , [0, 0, 0]
                                                       , [0, 0, 0]
                                                       , [0, 0, 0]
                                                       ]
            Nothing `shouldBe` listBoard [ [0, 0, 0]
                                         , [0, 0, 0] 
                                         , [0, 0]
                                         ]
            let
                l = [ [1, 2, 0]
                    , [0, 1, 2]
                    , [2, 1, 0]
                    , [0, 1, 2]
                    ]
                Just rb = listBoard l
                r = toLists rb
                in l `shouldBe` r
            
            Nothing `shouldBe` listBoard [[0, 1, 3]]

    describe "putElem" $
        it "should return a puted board from elem and a origin board" $
            let 
            origin = emptyBoard 3 4
            next = listBoard [ [0, 0, 0]
                             , [0, 0, 1]
                             , [0, 0, 0]
                             , [0, 0, 0]
                             ]
            in putElem origin ((2,1), 1) `shouldBe` next

    describe "putElems" $  
        it "should return a puted board from elems and a origin board" $
            let 
            Just origin = listBoard [ [1, 0, 0]
                                    , [0, 0, 2]
                                    , [0, 1, 0]
                                    , [0, 0, 0]
                                    ] 
            puted = putElems origin [ ((0, 0), 0)
                                    , ((2, 1), 1)
                                    , ((0, 1), 2)
                                    ] 
            in puted `shouldBe` listBoard [ [0, 0, 0]
                                          , [2, 0, 1]  
                                          , [0, 1, 0]  
                                          , [0, 0, 0]  
                                          ]

