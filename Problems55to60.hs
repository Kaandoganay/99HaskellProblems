data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

--Problem55--
cbalTree :: Integral t => t -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n |  odd n = [Branch 'x' lc rc| lc <- cbalTree ((n-1) `div` 2), rc <- cbalTree ((n-1) `div` 2)]
           | otherwise = [Branch 'x' lc rc| lc <- cbalTree (n `div` 2), rc <- cbalTree ((n-1) `div` 2)]


--Problem56--
size :: Tree a -> Int
size Empty = 0
size (Branch _ lc rc ) = 1 + size lc + size rc

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch  _ lc rc) | size lc == size rc = True
                            | otherwise = False

--Problem57--

add :: Ord a => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x (Branch y lc rc)
  | x < y  = Branch y (add x lc) rc
  | x >= y = Branch y lc (add x rc)

construct :: (Foldable t, Ord a) => t a -> Tree a
construct = foldl (flip add) Empty

--Problem58--

symCbalTrees :: Integer -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree