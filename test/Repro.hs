module Main where

import AOC2025 (Instruction(..), Direction(..))

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

main :: IO ()
main = do
    let instructions = 
            [ Instr L 68
            , Instr L 30
            , Instr R 48
            , Instr L 5
            , Instr R 60
            , Instr L 55
            , Instr L 1
            , Instr L 99
            , Instr R 14
            , Instr L 82
            ]
    
    let (finalPos, total) = foldl step2 (50, 0) instructions
    
    putStrLn $ "Final Pos: " ++ show finalPos
    putStrLn $ "Total Crossings: " ++ show total
    
    if total == 6 
        then putStrLn "Test Passed!" 
        else putStrLn "Test Failed!"
