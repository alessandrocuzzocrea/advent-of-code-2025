module Day03 (part1, part2) where

part1 :: String -> Int
part1 input = sum $ map solveBank (lines input)

solveBank :: String -> Int
solveBank s = 
    let -- find max digit excluding last char (since y must follow x)
        candidatesX = init s
        maxX = maximum candidatesX
        
        -- find first occurrence of maxX to maximize remaining suffix for y
        (_, matchAndAfter) = break (== maxX) s
        
        -- find max digit in the suffix after maxX
        candidatesY = tail matchAndAfter
        maxY = maximum candidatesY
        
    in read [maxX, maxY]

part2 :: String -> Int
part2 _ = 0
