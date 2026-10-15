{-# LANGUAGE LambdaCase #-}
double :: [Int] -> [Int]
double = \case
    [] -> []
    (x:xs) -> 2 * x : double xs
