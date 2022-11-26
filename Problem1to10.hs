
myLast :: [a] -> a
myLast = last


myButLast :: [a] -> a
myButLast xs = last (init xs)