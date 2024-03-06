import Common

sigma2 :: Integer -> Integer
sigma2 = sum . map (^2) . divisors

-- doesn't work for 5 :P
bigSigma2 :: Integer -> Integer
bigSigma2 n =
    let s1 = sum $ map (\i -> (n `div` i) * i^2) $ takeWhile (\i -> i^2 <= n) [1..]
        s2'1 = takeUntil (\i -> i^2 >= n || i*2 >= n) [1..]
        s2'2 = (tail >>= zipWith (\i j -> (square(n `div` j) - square(n `div` i)) * j)) s2'1
        s2 = sum s2'2
    in  s1 + s2

main = do
    let ans = bigSigma2 (10^15)
    print ans
