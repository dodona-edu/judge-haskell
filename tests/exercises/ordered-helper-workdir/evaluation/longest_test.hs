import Test.HUnit
import HUnitJudge
import qualified Helper as H
import Input

main :: IO ()
main = do
    text <- readFile "words.txt"
    runJSON [ TestCase $ isEqual "longestWord <words.txt>" (H.longestWord text) (longestWord text) ]
