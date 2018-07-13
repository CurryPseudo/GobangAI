module AI.PatternSpec where

import Data.Char
import Data.List

import Test.Hspec
import Test.QuickCheck

import AI.Pattern
import Gobang

sr :: String -> ([[Int]], Int)
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
spec =  
    describe "chessesJudge" $ do
        it "return match patterns from a chess sequence" $ do
          chessesJudgePatternsTest "011201" `shouldBe` ["011"]
          chessesJudgePatternsTest "011121110" `shouldBe` ["0111", "1110"]
          chessesJudgePatternsTest "0112201010" `shouldBe` ["011", "01010", "220"] 
          chessesJudgePatternsTest "111" `shouldBe` [] 
          chessesJudgePatternsTest "01101020230" `shouldBe` ["011010", "0202"] 
          chessesJudgePatternsTest "" `shouldBe` []
          chessesJudgePatternsTest "011111" `shouldBe` ["011111"]
          chessesJudgePatternsTest "011011" `shouldBe` ["011011"]
          chessesJudgePatternsTest "01101" `shouldBe` ["01101"]
          chessesJudgePatternsTest "011010110" `shouldBe` ["011010", "0110"]
          chessesJudgePatternsTest "01101110110" `shouldBe` ["011011", "10110"]
          chessesJudgePatternsTest "011011110220" `shouldBe` ["0110", "011110", "0220"]
          chessesJudgePatternsTest "00111" `shouldBe` ["0111"]
        it "return patterns contained in itself" $ property $
            forAll (resize 50 (listOf (choose (0, 2)))) $ \xs ->
                let
                    patterns = fst (chessesJudge xs)
                in all (`isInfixOf` xs) patterns

        it "return a compareable num from a chess sequence" $ do
          chessesJudgeScoresComp "01101" "011011" `shouldBe` LT
          chessesJudgeScoresComp "011010110" "01101110110" `shouldBe` LT
          chessesJudgeScoresComp "021010" "022110" `shouldBe` GT
          chessesJudgeScoresComp "0220201110" "0111022020" `shouldBe` EQ
        it "return proper score" $ property $
            forAll (resize 50 (listOf (choose (0, 2)))) $ \xs ->
                let
                    score ys = snd (chessesJudge ys)
                    wrap c = [c:xs, xs ++ [c], c:xs ++ [c]]
                    tl = [ (1, (>=))
                         , (2, (<=))
                         ]
                    valid (c, comp) = all (\ys -> score ys `comp` score xs) (wrap c)
                in all valid tl
