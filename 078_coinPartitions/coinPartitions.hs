import Common

import Data.Array (Array, (!), array)
import qualified Data.Array as Array

n :: Integer
n = 10000

coins :: Array (Integer,Integer) Integer
coins = array ((0,1),(n,n))
    [ ((num,denom),ways)
    | num <- [1..n]
    , denom <- [1..n]
    , let ways
            | num < denom = 0
            | num > denom = coins!(num - denom, denom) + coins!(num, denom + 1)
            | otherwise = 1
    ]

main :: IO ()
main = do
    --let ans = coins!(n,1)
    print $ coins!(n,1)
