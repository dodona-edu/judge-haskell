double :: [Int] -> [Int]
double = map (* 2)

main :: IO ()
main = print (double [1, 2, 3])
