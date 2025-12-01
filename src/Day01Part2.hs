module Day01Part2 (runPart2) where

import AOC2025 (Instruction(..), Direction(..), parse)

dialSize :: Int
dialSize = 100

normalize :: Int -> Int
normalize x = (x `mod` dialSize + dialSize) `mod` dialSize

-- Count how many times you hit 0 while moving from start by 'steps'
crossesZero :: Int -> Int -> Int
crossesZero start steps
    | steps == 0 = 0
    | otherwise  =
        let end = start + steps
            fullCycles = abs steps `div` dialSize
            leftover   = abs steps `mod` dialSize

            dir = if steps > 0 then 1 else -1

            -- check if leftover path crosses zero
            s = start
            e = start + dir * leftover

            leftoverCross =
                if dir > 0
                    then if s < 0 && e >= 0 then 1 else if s > e then 1 else 0
                    else if s > 0 && e <= 0 then 1 else if e > s then 1 else 0
        in fullCycles + leftoverCross

step2 :: (Int, Int) -> Instruction -> (Int, Int)
step2 (pos, count) (Instr dir n) =
    let signed = case dir of
                    L -> -n
                    R ->  n
        zeroDuring = crossesZero pos signed
        pos'       = normalize (pos + signed)
        zeroEnd    = if pos' == 0 then 1 else 0
    in (pos', count + zeroDuring + zeroEnd)

runPart2 :: String -> Int
runPart2 input =
    let instructions = map parse (lines input)
        (_, total)   = foldl step2 (50, 0) instructions
    in total
