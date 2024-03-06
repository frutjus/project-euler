module Common where

properDivisors :: Integer -> [Integer]
properDivisors n = 1 : concat
    [ i : [n `div` i | i ^ 2 /= n]
    | i <- takeWhile (\i' -> i' ^ 2 <= n) [2..]
    , n `mod` i == 0
    ]

divisors :: Integer -> [Integer]
divisors n = concat
    [ i : [n `div` i | i ^ 2 /= n]
    | i <- takeWhile (\i' -> i' ^ 2 <= n) [1..]
    , n `mod` i == 0
    ]

primes :: [Integer]
primes = 2 : f [3,5..]
    where f (x:xs) = x : (f . filter (\n -> n `mod` x /= 0)) xs

isPrime :: Integer -> Bool
isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p <= (floor . sqrt . fromIntegral) n) primes

primeFactors :: Integer -> [(Integer,Integer)]
primeFactors n
    | n < 1 = []
    | otherwise = fn n primes []
    where fn 1 _ fs = fs
          fn n pss@(p:ps) fs
            | n `mod` p == 0 = fn (n `div` p) pss fss
            | otherwise = fn n ps fs
            where fss = case fs of
                    [] -> [(p,1)]
                    (f,c):fs' -> if f == p
                                    then (f,c+1):fs'
                                    else (p,1):fs

triangular :: Integer -> Integer
triangular n = n * (n + 1) `div` 2

square :: Integer -> Integer
square n = n * (n + 1) * (2*n + 1) `div` 6

takeUntil' :: (a -> Bool) -> [a] -> [a]
takeUntil' p xs = reverse $ f p xs []
    where f p [] ys = ys
          f p (x:xs) ys | p x = x:ys
                        | otherwise = f p xs (x:ys)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) | p x = [x]
                   | otherwise = x : takeUntil p xs

fibonacci :: [Integer]
fibonacci = map fst $ iterate (\(a,b) -> (a+b,a)) (0,1)

squareFree :: Integer -> Bool
squareFree = not . any ((/=1) . snd) . primeFactors
