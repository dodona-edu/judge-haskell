half :: Int -> Int
half n = n `div` 2

power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n - 1)

(|+|) :: Int -> Int -> Int
x |+| y = abs x + abs y

clamp :: Int -> Int -> Int -> Int
clamp low high x
    | x < low = low
    | x > high = high
    | otherwise = x
