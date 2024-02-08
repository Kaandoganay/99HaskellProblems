{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import System.Random

--Problem21--
insert :: a -> [a] -> Int -> [a]
insert a xs n = take (n-1) xs ++ a: drop n xs

--Problem22--

range :: (Num a, Enum a) => a -> a -> [a]
range x y = [x,x+1..y]

--Problem23--
rndselect xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, length xs - 1) gen]

--Problem24--
diff_select n x= rndselect [1..x] n