module Day01Spec (spec) where

import Test.Hspec
import AOC2025

spec :: Spec
spec = do
    describe "parse" $ do
        it "parses L68" $
            parse "L68" `shouldBe` Instr L 68

        it "parses R5" $
            parse "R5" `shouldBe` Instr R 5

    describe "step" $ do
        it "moves correctly to the left" $
            step (11, 0) (Instr L 8) `shouldBe` (3, 0)

        it "moves correctly to the right" $
            step (11, 0) (Instr R 8) `shouldBe` (19, 0)

        it "wraps around on left" $
            step (5, 0) (Instr L 10) `shouldBe` (95, 0)

        it "wraps around on right" $
            step (99, 0) (Instr R 5) `shouldBe` (4, 0)

        it "counts zero hits" $
            step (52, 0) (Instr R 48) `shouldBe` (0, 1)

    describe "full example" $ do
        it "produces the correct count for the sample" $ do
            let instructions =
                    [ Instr L 68, Instr L 30, Instr R 48, Instr L 5, Instr R 60
                    , Instr L 55, Instr L 1, Instr L 99, Instr R 14, Instr L 82
                    ]
                (_, result) = foldl step (50, 0) instructions
            result `shouldBe` 3
