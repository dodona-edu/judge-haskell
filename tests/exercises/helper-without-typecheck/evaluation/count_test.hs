import Test.HUnit
import HUnitJudge
import qualified Helper as H
import Input

main :: IO ()
main = runJSON [ TestCase $ isEqual "countWords \"a b c\"" (H.countWords "a b c") (countWords "a b c") ]
