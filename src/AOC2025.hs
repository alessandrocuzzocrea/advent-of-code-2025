module AOC2025
  ( Instruction(..)
  , Direction(..)
  , parse
  ) where

data Direction = L | R
  deriving (Show, Eq)

data Instruction = Turn Direction Int
  deriving (Show, Eq)

parse :: String -> Instruction
parse s =
    case words s of
        [dirStr, nStr] ->
            let dir = case dirStr of
                        "L" -> L
                        "R" -> R
                        _   -> error ("Invalid direction: " ++ dirStr)
                n = read nStr
            in Turn dir n
        _ -> error ("Invalid instruction: " ++ s)
