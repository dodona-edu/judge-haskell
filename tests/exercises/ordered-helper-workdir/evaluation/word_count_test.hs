import Test.HUnit
import HUnitJudge
import qualified Helper as H
import Input

main :: IO ()
main = do
    text <- readFile "words.txt"
    runJSON
        [ TestCase $ isEqual "countWords <words.txt>" (H.countWords text) (countWords text)
        , TestCase $ isEqual "countWords \"\"" (H.countWords "") (countWords "")
        ]
