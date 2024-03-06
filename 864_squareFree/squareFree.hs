primes :: [Integer]
primes = 2 : f [3,5..]
    where f (x:xs) = x : (f . filter (\n -> n `mod` x /= 0)) xs

isPrime :: Integer -> Bool
isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p <= (floor . sqrt . fromIntegral) n) primes
