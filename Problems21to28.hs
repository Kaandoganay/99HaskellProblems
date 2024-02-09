{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.List ( nub, subsequences )
import System.Random

--Problem21--
insert :: a -> [a] -> Int -> [a]
insert a xs n = take (n-1) xs ++ a: drop n xs

--Problem22--

range :: (Num a, Enum a) => a -> a -> [a]
range x y = [x,x+1..y]

--Problem23--

rndselect :: Int -> [a] -> [a]
rndselect n x = map (x!!) is
 where is = take n . nub $ randomRs (0, length x - 1) (mkStdGen 100)
--Problem24--

diffselect :: (Num a, Enum a) => Int -> a -> [a]
diffselect x y = rndselect x [1..y]

--Poblem25--
rndPermu :: [a] -> [a]
rndPermu xs = rndselect (length xs ) xs

--Problem26--
combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) (subsequences ns)

--Problem27--
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs =
    [ g:gs | (g,rs) <- combination n xs
           ,  gs    <- group ns rs ]

altogether :: [Int] -> [a] -> Int
altogether (x:xs) xz = length (group (x:xs) xz)