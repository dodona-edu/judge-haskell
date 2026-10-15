double :: [Int] -> [Int]
double xs = map (\x -> (x * 2)) xs

isPositive :: Int -> Bool
isPositive x =
    if x > 0
        then True
        else False
