--Problem1--
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
myLast :: [a] -> a
myLast = last

--Problem2--
myButLast :: [a] -> a
myButLast xs = last (init xs)

--Problem3--
elementAt :: [a] -> Int -> a
elementAt xs n = last (take n xs)

--problem4--
myLength :: [a] -> Int
myLength = length

--Problem5--
myReverse :: [a] -> [a]
myReverse xs = reverse xs

--Problem6--
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs =if xs == reverse xs then True else False

--Problem7 :  Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).--