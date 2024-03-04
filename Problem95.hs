module Problem95 where

numbers :: [String]
numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

fullWords n | n < 10 = numbers !!(n-1)
            | n < 100 = fullWords (n `div` 10) ++ "-" ++ fullWords (n `mod` 10)
            | n < 1000 = fullWords (n `div` 100) ++ "-" ++ fullWords (n `mod` 100) 