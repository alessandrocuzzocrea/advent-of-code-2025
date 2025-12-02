module AOC2025
  ( Instruction(..)
  , Direction(..)
  , parse
  , step
  ) where

import Data.Char (isDigit)

data Direction = L | R
  deriving (Show, Eq)

data Instruction = Instr Direction Int
  deriving (Show, Eq)

parse :: String -> Instruction
parse (c:rest) = 
    let dir = case c of
                'L' -> L
                'R' -> R
                _   -> error "Invalid direction"
        n = read (filter isDigit rest)
    in Instr dir n
parse _ = error "Invalid input"

type Pos = Int

-- apply a rotation and count zero hits (part 1 logic)
step :: (Pos, Int) -> Instruction -> (Pos, Int)
step (pos, count) (Instr dir dist) =
    let newPos = case dir of
                    L -> (pos - dist) `mod` 100
                    R -> (pos + dist) `mod` 100
        count' = if newPos == 0 then count + 1 else count
    in (newPos, count')
