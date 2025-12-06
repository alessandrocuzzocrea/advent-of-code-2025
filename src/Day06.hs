module Day06 (part1, part2) where

import Data.List (transpose, dropWhileEnd)
import Data.Char (isSpace, isDigit)

part1 :: String -> Int
part1 input = sum $ map solveProblem (splitProblems input)

splitProblems :: String -> [String]
splitProblems input =
    let lines' = lines input
        maxLength = maximum (map length lines')
        paddedLines = map (padRight maxLength) lines'
        cols = transpose paddedLines
        -- Split cols by empty columns
        problemCols = splitByEmpty cols
    in map (unlines . transpose) problemCols

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

splitByEmpty :: [String] -> [[String]]
splitByEmpty [] = []
splitByEmpty cols =
    let (chunk, rest) = break (all isSpace) cols
        rest' = dropWhile (all isSpace) rest
    in if null chunk
       then splitByEmpty rest'
       else chunk : splitByEmpty rest'

solveProblem :: String -> Int
solveProblem s =
    let allWords = words s
        operator = head $ filter (\w -> w == "+" || w == "*") allWords
        numbers = map read $ filter (all isDigit) allWords
    in case operator of
        "+" -> sum numbers
        "*" -> product numbers
        _ -> 0

part2 :: String -> Int
part2 _ = 0
