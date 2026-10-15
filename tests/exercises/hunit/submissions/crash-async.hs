import Control.Exception (AsyncException (StackOverflow), throw)

double :: [Int] -> [Int]
double xs = throw StackOverflow
