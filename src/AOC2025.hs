module AOC2025 (parse, step) where

import Data.Char (isDigit)

type Pos = Int

parse :: String -> (Char, Int)
parse (c:rest) = (c, read (filter isDigit rest))
parse _ = error "Invalid input"

step :: (Pos, Int) -> (Char, Int) -> (Pos, Int)
step (pos, count) (dir, dist) =
    let newPos = case dir of
                    'L' -> (pos - dist) `mod` 100
                    'R' -> (pos + dist) `mod` 100
                    _   -> error "Invalid direction"
        count' = if newPos == 0 then count + 1 else count
    in (newPos, count')
