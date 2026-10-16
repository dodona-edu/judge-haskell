import Test.HUnit
import HUnitJudge
import Input

main :: IO ()
main = runJSON
    [ TestCase $ isEqual "double []" [] (double [])
    , TestCase $ isEqual "double [1, 2, 3]" [2, 4, 6] (double [1, 2, 3])
    , TestCase $ isEqual "double [-5]" [-10] (double [-5])
    ]
