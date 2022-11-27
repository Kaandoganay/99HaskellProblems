
myLast :: [a] -> a
myLast = last


myButLast :: [a] -> a
myButLast xs = last (init xs)


elementAt :: [a] -> Int -> a
elementAt xs n = last (take n xs)

myLength :: [a] -> Int
myLength = length

myReverse :: [a] -> [a]
myReverse = reverse


isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = if xs == reverse xs then True else False


