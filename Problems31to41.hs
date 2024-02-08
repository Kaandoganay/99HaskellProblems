import Control.Concurrent ()

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

primefactors :: Int -> [Int]
primefactors x |x <= 0 = error "Value should be positive"
               |x ==1 = [1]
               |otherwise = filter isprime [k | k<-[2..x], x `mod` k ==0 ]

--Problem36--