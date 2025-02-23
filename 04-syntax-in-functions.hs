-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9
import Data.List ((!?))
import Data.Maybe (fromMaybe)
englishDigit :: Int -> String
englishDigit = fromMaybe "unknown" . (!?) ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

-- given a tuple, divide fst by snd, using pattern matching. 
-- it should return undefined for division by zero
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, y)
  | y == 0 = undefined
  | otherwise = x / y

-- if the first three numbers in a list are all zero, return True
threeZeroList :: [Int] -> Bool
threeZeroList (0:0:0:_) = True
threeZeroList _ = False
