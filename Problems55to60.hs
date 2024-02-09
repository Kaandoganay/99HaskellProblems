data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

--Problem55--

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

cbalTree :: Integral t => t -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [leaf 'x']
cbalTree n |  n `mod` 2 == 1 =[ Branch 'x' l r | l <- cbalTree ((n - 1) `div` 2),  r <- cbalTree ((n - 1) `div` 2) ] 
           | otherwise = concat [ [Branch 'x' l r, Branch 'x' r l] | l <- cbalTree ((n - 1) `div` 2), r <- cbalTree (n `div` 2) ]

--problem56--

mirror :: Tree a1 -> Tree a2 -> Bool
mirror    Empty              Empty     = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror   _              _            = False

symmetric :: Tree a2 -> Bool
symmetric t = mirror t t

--Problem57--
