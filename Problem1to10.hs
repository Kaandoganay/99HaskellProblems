module Problem1to10 where

import Data.List ( group )


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
isPalindrome xs =xs == reverse xs

--Problem7--

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

--Problem8--

compress :: Eq a => [a] -> [a]
compress x = foldr (\a b -> if a== head b then b else a:b  ) [last x] x

--Problem9--

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

--Problem10--

encode :: Eq b => [b] -> [(Int, b)]
encode xs = map (\x -> (length x,head x)) (group xs)
