import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)

double :: [Int] -> [Int]
double xs = unsafePerformIO exitFailure
