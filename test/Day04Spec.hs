module Day04Spec (spec) where

import Test.Hspec
import Day04

exampleInput :: String
exampleInput = unlines
    [ "..@@.@@@@."
    , "@@@.@.@.@@"
    , "@@@@@.@.@@"
    , "@.@@@@..@."
    , "@@.@@@@.@@"
    , ".@@@@@@@.@"
    , ".@.@.@.@@@"
    , "@.@@@.@@@@"
    , ".@@@@@@@@."
    , "@.@.@@@.@."
    ]

spec :: Spec
spec = do
    describe "part1" $ do
        it "matches the example" $
            part1 exampleInput `shouldBe` 13

    describe "part2" $ do
        it "matches the example" $
            part2 exampleInput `shouldBe` 43
