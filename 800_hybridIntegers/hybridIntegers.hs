import Control.Monad (when, forM_)
import Data.Ratio

primes :: [Integer]
primes = 2 : f [3,5..]
    where f (x:xs) = x : (f . filter (\n -> n `mod` x /= 0)) xs

isPrime :: Integer -> Bool
isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p <= (floor . sqrt . fromIntegral) n) primes

primeFactors :: Integer -> [(Integer,Integer)]
primeFactors n = fn n primes []
    where fn 1 _ fs = fs
          fn n pss@(p:ps) fs
            | n `mod` p == 0 = fn (n `div` p) pss fss
            | otherwise = fn n ps fs
            where fss = case fs of
                    [] -> [(p,1)]
                    (f,c):fs' -> if f == p
                                    then (f,c+1):fs'
                                    else (p,1):fs

hybridIntegers :: [[(Integer,Integer,Integer)]]
hybridIntegers = map (\p -> map (\q -> (p,q,p^q * q^p)) $ takeWhile (<p) primes) $ tail primes

belowN :: [(Integer,Integer,Integer)]
belowN = concat $ takeWhile (not . null) $ map (takeWhile (\(_,_,p) -> p <= n)) hybridIntegers

ans :: Int
ans = length belowN

n :: Integer
n = 800800^800800

main = do
    print ans
