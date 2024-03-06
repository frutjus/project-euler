import Control.Monad (forM, when)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe (isJust, catMaybes)

properDivisors :: Integer -> [Integer]
properDivisors n = 1 : concat
    [ i : [n `div` i | i /= sqrtn]
    | i <- [2..sqrtn]
    , n `mod` i == 0
    ] where sqrtn = floor $ sqrt $ fromIntegral n

sumProperDivisors :: Integer -> Integer
sumProperDivisors = sum . properDivisors

amicableChain :: Integer -> Maybe [Integer]
amicableChain n = aux [n]
    where aux ns@(n:_) = let n' = sumProperDivisors n in if n' > 1000000 then Nothing else if n' `elem` ns then Just (n' : takeWhile (/=n') ns) else aux (n':ns)

main = do
    chains <- forM [1..1000000] $ \n -> do
        let chain = amicableChain n
        when (fmap length chain > Just 2) $
            putStrLn $ show n ++ ": " ++ maybe "oob" show chain
        return $! chain
    let chains' = catMaybes chains
    let longestChain = maximumBy (comparing length) chains'
    let smallestNumber = minimum longestChain
    putStrLn $ "Longest chain: " ++ show longestChain
    putStrLn $ "Smallest member: " ++ show smallestNumber
