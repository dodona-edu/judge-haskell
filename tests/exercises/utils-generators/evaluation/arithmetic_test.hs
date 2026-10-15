import Test.HUnit
import HUnitJudge
import Utils
import qualified Input as I

half :: Int -> Int
half n = n `div` 2

power :: Int -> Int -> Int
power = (^)

(|+|) :: Int -> Int -> Int
x |+| y = abs x + abs y

clamp :: Int -> Int -> Int -> Int
clamp low high = max low . min high

main :: IO ()
main = runJSON $
    generateTest1 "half" half I.half [7, -3]
    ++ generateTest2 "power" power I.power (combos2 [2, 3] [0, 2])
    ++ generateInfix "|+|" (|+|) (I.|+|) [(-1, 2)]
    ++ generateTest3 "clamp" clamp I.clamp [(0, 10, -5), (0, 10, 15)]
