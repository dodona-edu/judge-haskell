{-# LANGUAGE LambdaCase #-}
module Double
    ( double
    ) where

double :: [Int] -> [Int]
double = \case
    [] -> []
    (x:xs) -> 2 * x : double xs +++ []
