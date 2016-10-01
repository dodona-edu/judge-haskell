--   ___ ___      ____.         .___
--  /   |   \    |    |__ __  __| _/ ____   ____
-- /    ~    \   |    |  |  \/ __ | / ___\_/ __ \
-- \    Y    /\__|    |  |  / /_/ |/ /_/  >  ___/
--  \___|_  /\________|____/\____ |\___  / \___  >
--        \/                     \/_____/      \/

--
-- This is a custom judge for the haskell platform
-- Currently the judge is very minimal and only supports
-- isEqual and complete failure
--

module HJudge where

import Test.HUnit
import Control.Monad
import Data.String.Utils

--
-- Helper function so that we can intercept
-- generation of messages by HUnit
--
reportMsg :: String -> Bool -> Int -> IO Int
reportMsg message isProgress count = do
  putStrLn $ if isProgress then  message else ""
  return (count+1)

myPutText = PutText reportMsg 0  :: PutText Int

--
-- Generate a json string of accepted or failed tests
--
makeOutput failed descr test =
 "{"++
 "        \"accepted\": "++ failed ++ ","++
 "        \"groups\": [{"++
 "            \"accepted\": "++ failed ++","++
 "            \"description\": \"" ++ quoteJSON descr ++ "\","++
 "            \"tests\": [ " ++ test ++ "]"++
 "        }]"++
 "}"

--
-- Used by HUnit to generate output for an equality test
--
isEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
isEqual preface expected actual =
  if actual == expected then assertFailure msgOk else assertFailure msgFail
    where msgFail = makeOutput "false" preface $
                   "{ \"accepted\": false,\n" ++
                   "\"expected\": \"" ++ quoteJSON (show expected) ++
                   "\",\n\"generated\": \"" ++ quoteJSON (show actual) ++ "\"\n } \n"
          msgOk  = makeOutput "true" preface $
                   "{ \"accepted\": true,\n" ++
                   "\"expected\": \"" ++ quoteJSON (show expected) ++
                   "\",\n\"generated\": \"" ++ quoteJSON (show actual) ++ "\"\n } \n"

-- Replace all newlines by quoted newlines
quoteJSON = (replace "\"" "\\\"") . (replace "\n" "\\n")


isLast state = (cases $ counts state) == (tried $ counts state)

seperator ss = sep
    where sep = if isLast ss then "" else ","

makeCrash msg = makeOutput "false" "Kan oefening niet evalueren, waarschijnlijk zit er nog een fout in je code." $ 
   "{ \"accepted\": false,\n" ++  
   "\"expected\": \"" ++ " " ++  
   "\",\n\"generated\": \"" ++  quoteJSON msg ++ "\"\n } \n"

makeTests list = TestList $ map (TestLabel "")   list

runJSON list = runTestJSON myPutText (makeTests list)

runTestJSON :: PutText st -> Test -> IO ()
runTestJSON (PutText put us0) t = do
  putStrLn "["
  (counts', us1) <- performTest reportStart reportError reportFail us0 t
  putStrLn "]"
  return ()
 where
  reportStart ss = put "" False
  reportFail  loc msg ss = put (msg ++ (seperator ss)) True
  reportError loc msg ss = put (makeCrash  msg ++ (seperator ss)) True

