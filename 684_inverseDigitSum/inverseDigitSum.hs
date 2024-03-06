s :: Integer -> Integer
s n = 10 ^ (n `div` 9) * (n `mod` 9 + 1) - 1
