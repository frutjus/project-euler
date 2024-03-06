seed :: Integer
seed = 1504170715041707

modulus :: Integer
modulus = 4503599627370517

eulerSequence :: [Integer]
eulerSequence = iterate (\i -> (i + seed) `mod` modulus) seed

runningMin :: [Integer]
runningMin = modulus : scanl1 min eulerSequence

eulerCoins :: [Integer]
eulerCoins = takeUntil (==1) $ map fst $ filter (uncurry (<)) $ zip eulerSequence runningMin

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
    | p x = [x]
    | otherwise = x : takeUntil p xs

main = do
    mapM_ print eulerCoins
    let ans = sum eulerCoins
    putStrLn $ "Sum of Eulercoins = " ++ show ans