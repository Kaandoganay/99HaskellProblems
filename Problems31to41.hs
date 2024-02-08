
--Problem31--
isprime :: Integer -> Bool
isprime x = null [k | k<-[2..(x-1)], x `mod` k == 0]

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
totient x = length [k | k <- [1..(x-1)], coprime k x ]

--Problem35--

primefactors :: Integer -> [Integer]
primefactors x = filter isprime [k | k<-[2..x], x `mod` k ==0 ]