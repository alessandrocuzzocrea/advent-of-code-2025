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

-- Part 2: Invalid IDs are formed by repeating a sequence X at least twice.
-- N = X concatenated k times, where k >= 2.
-- We iterate over possible lengths of X (len) and possible repetition counts (k).
-- The total length of N will be len * k.
-- We only need to consider N <= hi.

invalidsInRangePart2 :: (Int, Int) -> [Int]
invalidsInRangePart2 (lo, hi) =
    let 
        maxDigits = length (show hi)
        
        candidates = do
            -- Length of the base sequence X
            len <- [1 .. maxDigits `div` 2]
            
            -- Number of repetitions k
            -- Minimum k is 2.
            -- Max k is such that len * k <= maxDigits
            k <- [2 .. maxDigits `div` len]
            
            let startX = 10 ^ (len - 1)
                endX   = (10 ^ len) - 1
            
            x <- [startX .. endX]
            
            -- Construct N by repeating X, k times
            -- Example: X=12, k=3 -> 121212
            -- N = X * (10^(len*(k-1)) + ... + 10^0)
            -- But simpler to just construct it iteratively or mathematically
            
            let n = constructRepetition x len k
            return n
            
    in filter (\n -> n >= lo && n <= hi) candidates

constructRepetition :: Int -> Int -> Int -> Int
constructRepetition x len k =
    let shift = 10 ^ len
        go 0 acc = acc
        go i acc = go (i - 1) (acc * shift + x)
    in go (k - 1) x

part1 :: String -> Int
part1 input = 
    let ranges = parse input
        invalids = concatMap invalidsInRange ranges
    in sum invalids

part2 :: String -> Int
part2 input = 
    let ranges = parse input
        -- Use a Set or unique list to avoid double counting if multiple (len, k) produce same N?
        -- Actually, can a number be formed by repeating X k times AND Y m times?
        -- Example: 111111 (X=1, k=6) and (X=11, k=3) and (X=111, k=2).
        -- The problem says "made only of some sequence of digits repeated at least twice".
        -- So 111111 is definitely invalid. We just need to sum it once.
        -- The ranges are disjoint in the input? The problem says "ranges separated by commas".
        -- It doesn't explicitly say they are disjoint, but usually they are.
        -- However, within a single range, we might generate the same number multiple times.
        -- e.g. 111111 from X=1,k=6 and X=11,k=3.
        -- So we should deduplicate candidates per range.
        
        invalids = concatMap (unique . invalidsInRangePart2) ranges
    in sum invalids

unique :: [Int] -> [Int]
unique xs = 
    let sorted = mergeSort xs
    in nubSorted sorted

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = 
    let (left, right) = splitAt (length xs `div` 2) xs
    in merge (mergeSort left) (mergeSort right)
  where
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
        | x <= y    = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys

nubSorted :: Eq a => [a] -> [a]
nubSorted (x:y:xs)
    | x == y    = nubSorted (y:xs)
    | otherwise = x : nubSorted (y:xs)
nubSorted xs = xs
