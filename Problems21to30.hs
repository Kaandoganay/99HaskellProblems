module Problems21to30 where

--Problem21--
insert :: a -> [a] -> Int -> [a]
insert a xs n = take (n-1) xs ++ a: drop n xs

--Problem22--

range :: (Num a, Enum a) => a -> a -> [a]
range x y = [x,x+1..y]

--Problem23--