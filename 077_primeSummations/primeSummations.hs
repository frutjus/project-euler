import Common

import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map

n :: Integer
n = 100

coins :: Map (Integer,Integer) Integer
coins = Map.fromList
    [ ((num,denom),ways)
    | num <- [1..n]
    , denoms <- tails $ takeWhile (<=n) primes
    , not (null denoms)
    , let denom = head denoms
    , let ways
            | num < denom = 0
            | num > denom = coins!(num - denom, denom) +
                case denoms of
                    (_:d:ds) -> coins!(num, d)
                    _ -> 0
            | otherwise = 1
    ]

main :: IO ()
main = do
    print $ head $ filter (\(_,f) -> f >= 5000) $ map (\i -> (i,coins!(i,2))) [1..n]
