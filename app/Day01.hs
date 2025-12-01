module Main where

import System.IO
import AOC2025 (parse, step)

main :: IO ()
main = do
    input <- readFile "inputs/day01.txt"
    let instructions = map parse (lines input)
        (_, count)   = foldl step (50, 0) instructions
    print count
