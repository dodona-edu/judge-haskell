import Data.List (sortOn)

countWords :: String -> Int
countWords = length . words

longestWord :: String -> String
longestWord = last . sortOn length . words
