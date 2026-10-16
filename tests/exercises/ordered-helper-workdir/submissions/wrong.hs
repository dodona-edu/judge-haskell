countWords :: String -> Int
countWords = length . lines

longestWord :: String -> String
longestWord = head . words
