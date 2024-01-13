--Problem1--
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
myReverse = reverse

--Problem6--
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs




