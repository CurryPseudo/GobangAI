module AI.PatternSpec where

import Data.Char

import Test.Hspec

import AI.Pattern
import Gobang

sr :: (Num a) => String -> ([[Int]], a)
sr s = chessesJudge (map digitToInt s)

chessesJudgePatternsTest :: String -> [String]
chessesJudgePatternsTest s = let
    (ps, _) = sr s
    in map (map intToDigit) ps

chessesJudgeScoresComp :: String -> String -> Ordering
chessesJudgeScoresComp s1 s2 = let
    (_, s1_) = sr s1
    (_, s2_) = sr s2
    in compare s1_ s2_


spec :: Spec
spec = do 
    describe "chessesJudge" $ do
        it "return match patterns from a chess sequence" $ do
          chessesJudgePatternsTest "011201" `shouldBe` ["011", "01"]
          chessesJudgePatternsTest "011121110" `shouldBe` ["0111", "1110"]
          chessesJudgePatternsTest "0112201010" `shouldBe` ["011", "220", "01010"] 
          chessesJudgePatternsTest "111" `shouldBe` [] 
          chessesJudgePatternsTest "01101020230" `shouldBe` ["011010"] 
          chessesJudgePatternsTest "" `shouldBe` []
          chessesJudgePatternsTest "011111" `shouldBe` ["11111"]
          chessesJudgePatternsTest "011011" `shouldBe` ["0110", "11011", "011"]
          chessesJudgePatternsTest "01101" `shouldBe` ["01101"]
          chessesJudgePatternsTest "011010110" `shouldBe` ["011010", "0110"]
          chessesJudgePatternsTest "01101110110" `shouldBe` ["11011", "0110"]
          chessesJudgePatternsTest "011011110220" `shouldBe` ["0110", "011110", "0220"]
        it "return a compareable num from a chess sequence" $ do
          chessesJudgeScoresComp "01101" "011011" `shouldBe` LT
          chessesJudgeScoresComp "011010110" "01101110110" `shouldBe` LT
          chessesJudgeScoresComp "021010" "022110" `shouldBe` GT
          chessesJudgeScoresComp "0220201110" "0111022020" `shouldBe` EQ
