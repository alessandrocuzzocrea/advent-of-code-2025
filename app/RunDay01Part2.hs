import Day01Part2 (runPart2)

main :: IO ()
main = do
    input <- readFile "inputs/day01.txt"
    print (runPart2 input)
