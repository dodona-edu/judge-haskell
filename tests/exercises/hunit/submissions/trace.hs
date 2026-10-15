import Debug.Trace (trace)

double :: [Int] -> [Int]
double xs = trace ("double " ++ show xs) (map (* 2) xs)
