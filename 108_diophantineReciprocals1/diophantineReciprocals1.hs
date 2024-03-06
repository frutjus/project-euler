import Common

import Data.List ( minimumBy )
import Data.Ord ( comparing )
import GHC.IO ( unsafePerformIO )
import Control.Monad ( when )

numberOfDistinctSolutions :: Integer -> Int
numberOfDistinctSolutions = length . divisors

sols :: Integer -> Integer
sols n =
    let fs = primeFactors n
        es = map snd fs
    in  product (map (+1) es) + product es

ans :: [(Integer, Int)]
ans = takeUntil ((>1000) . snd) $ map (\i -> (i, numberOfDistinctSolutions i)) [1..]

ans' :: (Integer, Integer)
{-# NOINLINE ans' #-}
ans' = minimumBy (comparing fst) (f [])
    where
      f es
        | facts es >= 100 = unsafePerformIO $ do
            let ret = (num es, facts es)
            print (es, num es, facts es)
            return [ret]
        | otherwise = (if check es then f (incLast es) else [])
                   ++ f (es ++ [1])
      num es = product $ zipWith (^) primes es
      facts es = product (map (+1) es) + product es
      check [_] = True
      check [a,b] = b < a
      check (a:as) = check as
      check [] = False
      incLast [a] = [a+1]
      incLast (a:as) = a : incLast as
      incLast [] = error "Should never happen"

main = print ans'

main' = do
    mapM_ (
        \i -> when (fst i `mod` 1000 == 0) $ print i
     ) ans
    print $ last ans

-- >>> ans'
-- (21600,102)

-- >>> primeFactors 294_053_760
-- [(17,1),(13,1),(11,1),(7,1),(5,1),(3,3),(2,7)]

-- >>> primeFactors 245_044_800
-- [(17,1),(13,1),(11,1),(7,1),(5,2),(3,2),(2,6)]

-- >>> primeFactors 1260
-- [(7,1),(5,1),(3,2),(2,2)]

-- >>> sols 1260
-- 40

-- >>> divisors 30
-- [1,30,2,15,3,10,5,6]

-- >>> primeFactors 28
-- [(7,1),(2,2)]
