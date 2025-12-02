module Day02 (part1, part2) where

import Data.Char (isDigit)

-- parse "11-22,95-115" into [(11,22), (95,115)]
parse :: String -> [(Int, Int)]
parse input = 
    let clean c = if c == ',' || c == '-' || c == '\n' then ' ' else c
        nums = map read $ words $ map clean input
    in toPairs nums
  where
    toPairs (x:y:xs) = (x,y) : toPairs xs
    toPairs _ = []

-- generate invalid IDs (N = X concatenated with X) within a range by iterating possible lengths of X.

invalidsInRange :: (Int, Int) -> [Int]
invalidsInRange (lo, hi) =
    let 
        -- Max digits we need to consider. 
        -- If hi is 123456 (6 digits), we need to check X with length 1, 2, 3.
        -- If hi is 100 (3 digits), we check X with length 1.
        maxDigits = length (show hi)
        maxHalfLen = maxDigits `div` 2
        
        candidates = do
            len <- [1 .. maxHalfLen]
            let startX = 10 ^ (len - 1)
                endX   = (10 ^ len) - 1
            x <- [startX .. endX]
            let n = x * (10 ^ len) + x
            return n
            
    in filter (\n -> n >= lo && n <= hi) candidates

part1 :: String -> Int
part1 input = 
    let ranges = parse input
        invalids = concatMap invalidsInRange ranges
    in sum invalids

part2 :: String -> Int
part2 _ = 0
