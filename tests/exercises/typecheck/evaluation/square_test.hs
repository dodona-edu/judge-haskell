import Test.HUnit
import HUnitJudge
import Input

main :: IO ()
main = runJSON
    [ TestCase $ isEqual "square 3" 9 (square 3)
    , TestCase $ isEqual "square (-4)" 16 (square (-4))
    ]
