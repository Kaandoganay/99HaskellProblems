module Problems11to20 where

import Problem1to10 (encode)


--Problem11--
data ListItem a = Single a | Multiple Int a
           deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

--Problem12--

decodeModified :: [ListItem a] -> [a]
decodeModified = foldl (\acc e -> case e of Single x -> acc ++ [x]; Multiple n x -> acc ++ replicate n x) []

--problem14--


dupli :: [a] -> [a]
dupli [] = []
dupli xs = replicate 2 ( head xs) ++ dupli (tail xs)

--Problem15--

repli :: [a] -> Int -> [a]
repli [] n = []
repli xs n = replicate n (head xs) ++ repli (tail xs) n

--Proble16--

dropevery :: [a] -> Int -> [a]
dropevery xs n = take (n-1) xs ++ drop n xs

--Problem17--

split :: [a] -> Int -> ([a], [a])
split xs n = splitAt n xs

--problem18--

slice xs n m  = drop k (take l (unwords xs)
    where l = max n m
          k = min n m