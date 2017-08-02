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
import Data.String.Utils
import qualified Data.Json.Builder as JSON
import qualified Data.ByteString as BL
import Data.Monoid ((<>))
import qualified System.IO as IO


--
-- Helper function so that we can intercept
-- generation of messages by HUnit
--
reportMsg :: String -> Bool -> () -> IO ()
reportMsg message isProgress count = return count

myPutText :: PutText ()
myPutText = PutText reportMsg ()

--
-- Used by HUnit to generate output for an equality test
--
writeJSON :: JSON.Value a => a -> IO ()
writeJSON = BL.putStr . JSON.toJsonBS

isEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
isEqual preface should actual = do
    writeJSON $ JSON.row "command" "start-context"
    writeJSON $ JSON.row "command" "start-testcase"
             <> JSON.row "description" (JSON.row "format" "haskell" <> JSON.row "description" preface)
    writeJSON $ JSON.row "command" "start-test"
             <> JSON.row "expected" (show should)
    let accepted = should == actual
    if accepted then writeJSON $ JSON.row "command" "close-test"
                              <> JSON.row "generated" (show actual)
                              <> JSON.row "accepted" True
                              <> JSON.row "status" (JSON.row "enum" "correct" <> JSON.row "human" "Juist antwoord")
                else writeJSON $ JSON.row "command" "close-test"
                              <> JSON.row "generated" (show actual)
                              <> JSON.row "accepted" False
                              <> JSON.row "status" (JSON.row "enum" "wrong" <> JSON.row "human" "Fout antwoord")
    writeJSON $ JSON.row "command" "close-testcase"
    writeJSON $ JSON.row "command" "close-context"
    assertBool "" accepted

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

makeTests :: [Test] -> Test
makeTests list = TestList $ map (TestLabel "")   list

runJSON :: [Test] -> IO ()
runJSON list = do
    -- TODO: improve focus so I can set the tab status in here,
    -- something like {"command": "focus", "level": "tab"}
    runTestJSON myPutText (makeTests list)

runTestJSON :: PutText () -> Test -> IO ()
runTestJSON (PutText put us0) t = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  _ <- performTest reportStart reportError reportFail us0 t
  return ()
 where
  reportStart ss count = return ()
      -- TODO: open testcase with the top Label in `path ss`. This
      -- requires rewriting all tests with labels or somehow replacing
      -- isEqual with an Assertion I can get the preface from.
      -- The opened testcase should be successfull, with everything for
      -- a succeeding test filled in (but we can't get the actual?).
      -- putStrLn $ show $ path ss
  reportFail loc msg ss count = return ()
  reportError loc msg ss count = do
      writeJSON $ JSON.row "command" "close-test"
               <> JSON.row "accepted" False
               <> JSON.row "status" (JSON.row "enum" "runtime error" <> JSON.row "human" "Fout tijdens uitvoering")
               <> JSON.row "generated" ""
      writeJSON $ JSON.row "command" "close-testcase"
      writeJSON $ JSON.row "command" "close-context"
