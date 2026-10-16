import Graphics.Gloss
import Test.HUnit
import HUnitJudge
import Input

main :: IO ()
main = runJSON
    [ TestCase $ isEqual "target" (Pictures [Circle 10, Color red (Circle 20)]) target ]
