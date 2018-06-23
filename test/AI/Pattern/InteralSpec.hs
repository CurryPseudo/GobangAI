module AI.Pattern.InteralSpec where
import Test.Hspec
import AI.Pattern.Interal
import Data.Char

spec :: Spec
spec = do
    describe "fromString" $ do
        it "return proper patterns from a string" $
          fromString "1001101" `shouldBe` Just [ Ally
                                               , Empty
                                               , Empty
                                               , Ally
                                               , Ally
                                               , Empty
                                               , Ally
                                               ]

        it "return nothing from a invalid string" $
          fromString "141001" `shouldBe` Nothing

        it "return '' from a empty string" $
          fromString "" `shouldBe` Nothing
          
    describe "splitChesses" $ do
        it "split a chess sequence into multi part without block and enemy" $ do
          splitChesses [1,3,2,0,1,0,1,3,0,2] `shouldBe` [[1], [0,1,0,1], [0]]
          splitChesses [2] `shouldBe` []

    describe "chessesJudgeSingle" $ do
        it "return score from a chess sequence, not detecting enemy's score" $
          chessesJudgeSingle [0,1,1,2,2,0,1,0,1,0] `shouldBe` 15

main :: IO ()
main = hspec spec 


