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
 "            \"description\": {"++
 "                 \"format\": \"code\","++
 "                 \"description\": \"" ++ quoteJSON descr ++ "\""++
 "            },"++
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



---
--- Helper function to join strings with a space
---
s_concat :: [String] -> String
s_concat =  Data.String.Utils.join " "

---
--- Generation of testcases
---
judge1 :: (Show t, Show a, Eq a) => [Char] -> (t -> a) -> (t -> a) -> [t] -> [Test]
judge2 :: (Show t1, Show t, Show a, Eq a) => [Char] -> (t -> t1 -> a) -> (t -> t1 -> a) -> [(t, t1)] -> [Test]
judge3 :: (Show t2, Show t1, Show t, Show a, Eq a) => [Char] -> (t -> t1 -> t2 -> a) -> (t -> t1 -> t2 -> a) -> [(t, t1, t2)] -> [Test]
judge4 :: (Show t3, Show t2, Show t1, Show t, Show a, Eq a) => [Char] -> (t -> t1 -> t2 -> t3 -> a) -> (t -> t1 -> t2 -> t3 -> a) -> [(t, t1, t2, t3)] -> [Test]
judge1 msg fs fi input = [ TestCase (isEqual (s_concat [msg,show x])                      (fs x)       (fi x))       | x         <- input ]
judge2 msg fs fi input = [ TestCase (isEqual (s_concat [msg,show x,show y])               (fs x y)     (fi x y))     | (x,y)     <- input ]
judge3 msg fs fi input = [ TestCase (isEqual (s_concat [msg,show x,show y,show z])        (fs x y z)   (fi x y z))   | (x,y,z)   <- input ]
judge4 msg fs fi input = [ TestCase (isEqual (s_concat [msg,show x,show y,show z,show q]) (fs x y z q) (fi x y z q)) | (x,y,z,q) <- input ]


-- Replace all newlines by quoted newlines
-- Temporal hack to get rid of quoted string errors
quoteJSON = (replace "\"" "\\\"") . (replace "\n" "\\n") . filter (\x -> x /=  '\\' )

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
