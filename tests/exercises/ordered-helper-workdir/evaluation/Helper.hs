module Helper where

import Data.List (maximumBy)
import Data.Ord (comparing)

countWords :: String -> Int
countWords = length . words

longestWord :: String -> String
longestWord = maximumBy (comparing length) . words
