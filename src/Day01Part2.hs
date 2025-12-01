module Day01Part2 (runPart2) where

import AOC2025 (Instruction(..), Direction(..), parse)

dialSize :: Int
dialSize = 100

normalize :: Int -> Int
normalize x = (x `mod` dialSize + dialSize) `mod` dialSize

step2 :: (Int, Int) -> Instruction -> (Int, Int)
step2 (pos, count) (Instr dir n) =
    let signed = case dir of
                    L -> -n
                    R ->  n
        end = pos + signed
        
        crossings = if signed > 0
                    then (end `div` 100) - (pos `div` 100)
                    else ((pos - 1) `div` 100) - ((end - 1) `div` 100)
        
        pos' = normalize end
    in (pos', count + crossings)

runPart2 :: String -> Int
runPart2 input =
    let instructions = map parse (lines input)
        (_, total)   = foldl step2 (50, 0) instructions
    in total
