triangles :: [(Integer,Integer)]
triangles =
    [ (n,n+1) | n <- [4,6..333_333_333] ] ++
    [ (n,n-1) | n <- [4,6..333_333_333] ]

isValid :: (Integer,Integer) -> Bool
isValid (a,b) =
    let c = floor $ sqrt $ fromIntegral (b^2 - (a  `div` 2)^2)
    in  c^2 + (a `div` 2)^2 == b^2

perimeter :: (Integer, Integer) -> Integer
perimeter (a,b) = a + b*2

main = do
    let validTriangles = filter isValid triangles
    let ans = sum $ map perimeter validTriangles
    print ans