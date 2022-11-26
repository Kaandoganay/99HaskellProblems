
myLast :: [a] -> a
myLast = last


myButLast :: [a] -> a
myButLast xs = last (init xs)


elementAt :: [a] -> Int -> [a]
elementAt xs n = take n xs