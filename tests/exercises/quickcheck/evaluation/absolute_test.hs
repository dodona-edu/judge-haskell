import QuickCheckJudge
import Input

main :: IO ()
main = do
    executeTestCase $ TestCase "absolute n >= 0" (\n -> absolute n >= 0)
    executeTestCase $ TestCase "absolute (absolute n) == absolute n" (\n -> absolute (absolute n) == absolute n)
