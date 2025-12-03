module Day04Spec (spec) where

import Test.Hspec
import Day04

spec :: Spec
spec = do
    describe "part1" $ do
        it "works" $
            part1 "" `shouldBe` 0

    describe "part2" $ do
        it "works" $
            part2 "" `shouldBe` 0
