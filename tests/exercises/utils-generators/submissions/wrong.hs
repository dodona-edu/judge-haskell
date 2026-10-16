half :: Int -> Int
half n = n `quot` 2

power :: Int -> Int -> Int
power x n = x * n

(|+|) :: Int -> Int -> Int
x |+| y = x + y

clamp :: Int -> Int -> Int -> Int
clamp low high x
    | x < low = high
    | x > high = low
    | otherwise = x
