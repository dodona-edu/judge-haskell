double :: [Int] -> [Int]
double (x:xs) = 2 * x : double xs
