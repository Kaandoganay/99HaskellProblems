{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Control.Concurrent ()
import Problem1to10(encode)

--Problem31--
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isprime :: Int -> Bool
isprime x |x <=1 = error "Value should be bigger than 1"
          |null [k | k<-[2..(isqrt x)], x `mod` k == 0] = True
          |otherwise = False

--Problem32--
myGCD :: Integer -> Integer -> Integer
myGCD x y
      | y == 0     = abs x
      | otherwise  = myGCD y (x `mod` y)

--Problem33--

coprime :: Integer -> Integer -> Bool
coprime x y = myGCD x y == 1

--Problem34--

totient :: Integer -> Int
totient x | x<= 0  = error "Value should be positive"
          |otherwise = length [k | k <- [1..(x-1)], coprime k x ]

--Problem35--
primeFactors :: Int -> [Int]
primeFactors x
  | x <= 0    = error "Value should be positive."
  | x == 1    = []
  | otherwise = p : primeFactors (x `div` p)
    where p = head [n | n <- [2..x], x `mod` n == 0]

--Problem36--

prime_factors_mult ::  Int -> [(Int, Int)]
prime_factors_mult n = map swap $ encode $ primeFactors n
  where swap (x,y) = (y,x)

--Problem37--
totient' :: Int -> Int
totient' x = product [(a-1) * a ^ (b-1) | (a,b) <- prime_factors_mult x]

--Problem39--

primesR :: Int -> Int -> [Int]
primesR x y = [k |k <- [x .. y], isprime k]

--Problem40--
goldbach :: Int -> (Int, Int)
goldbach n | odd n = error "should be even number"
           | even n = head [(x, y) | x <- [2 .. n], isprime x, y <- [2 .. n], isprime y, x+y == n]

--Problem41--
evenList :: Int -> Int -> [Int]
evenList x y= [k | k<-[x..y], even k]

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList x y= map goldbach (evenList x y )

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' x y k = filter (\(a,b)-> a > k && b > k) $ goldbachList x y