module Problems46to50 where

--Problem46--

and' :: Bool -> Bool -> Bool
and' True True = True
and' _    _    = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _     _     = True

neg :: Bool -> Bool
neg True = False
neg False = True

nor :: Bool -> Bool -> Bool
nor a b = neg (or' a b)

nand' :: Bool -> Bool -> Bool
nand' a b = neg (and' a b)

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _      _   = False


impl' :: Bool -> Bool -> Bool
impl' a b =neg a `or'` b

equ' :: Bool -> Bool -> Bool
equ' True True   = True
equ' False False = True
equ' _     _     = False

table :: (Bool -> Bool-> Bool) -> IO()
table f = putStrLn $ unlines [show a ++ " " ++ show b ++ " " ++ show (f a b)| a <- [True, False], b <- [True, False] ]

--I am not going to solve Problems 47 and 48. They are almost identical to Problem 46.

--Problem49--

gray :: Integer -> [String]
gray 0 = [""]
gray n = ['0' : x | x <- gray (n-1)] ++ ['1': reverse x | x <- gray (n-1)]