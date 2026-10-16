double :: [Int] -> [Int]
double (x:xs) = x : double xs
