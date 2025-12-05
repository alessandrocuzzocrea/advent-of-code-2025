module Day05 (part1, part2) where

type Range = (Int, Int)

parse :: String -> ([Range], [Int])
parse input = 
    let (rangesStr, idsStr) = break (== "") (lines input)
        -- idsStr starts with the empty line from break so drop 1
        actualIdsStr = drop 1 idsStr
    in (map parseRange rangesStr, map read actualIdsStr)

parseRange :: String -> Range
parseRange s = 
    let (start, rest) = break (== '-') s
    in (read start, read (drop 1 rest))

part1 :: String -> Int
part1 input = 
    let (ranges, ids) = parse input
    in length $ filter (isFresh ranges) ids

isFresh :: [Range] -> Int -> Bool
isFresh ranges i = any (\(l, h) -> i >= l && i <= h) ranges

part2 :: String -> Int
part2 _ = 0
