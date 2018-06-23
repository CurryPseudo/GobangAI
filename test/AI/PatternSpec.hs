module AI.PatternSpec where

import Data.Char

import Test.Hspec

import AI.Pattern

chessesJudgeTest :: String -> Int
chessesJudgeTest s = chessesJudge (map digitToInt s)

spec :: Spec
spec = do 
    describe "chessesJudge" $ do
        it "return score from a chess sequence" $ do
          chessesJudgeTest "011201" `shouldBe` 6 
          chessesJudgeTest "011121110" `shouldBe` 80 
          chessesJudgeTest "0112201010" `shouldBe` 9 
          chessesJudgeTest "111" `shouldBe` 0 
          chessesJudgeTest "01101020230" `shouldBe` 65 
          chessesJudgeTest "" `shouldBe` 0
