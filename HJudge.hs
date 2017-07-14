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
import System.Exit (exitFailure, exitSuccess)


--
-- Helper function so that we can intercept
-- generation of messages by HUnit
--
reportMsg :: String -> Bool -> Int -> IO Int
reportMsg message isProgress count = return count

myPutText :: PutText Int
myPutText = PutText reportMsg 0

--
-- Used by HUnit to generate output for an equality test
--
writeJSON :: JSON.Value a => a -> IO ()
writeJSON = BL.putStr . JSON.toJsonBS

isEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
isEqual preface should actual = if actual == should
                                   then testcase True actual actual
                                   else testcase False should actual
    where
        testcase accepted expected generated = do
            writeJSON $ JSON.row "command" "new-context"
            writeJSON $ JSON.row "command" "set-properties"
                     <> JSON.row "accepted" accepted
            writeJSON $ JSON.row "command" "new-testcase"
            writeJSON $ JSON.row "command" "set-properties"
                     <> JSON.row "description" (JSON.row "format" "haskell"
                                             <> JSON.row "description" preface
                                               )
                     <> JSON.row "accepted" accepted
            writeJSON $ JSON.row "command" "new-test"
            writeJSON $ JSON.row "command" "set-properties"
                     <> JSON.row "accepted" accepted
                     <> JSON.row "expected" (show expected)
                     <> JSON.row "generated" (show generated)
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
    e <- runTestJSON myPutText (makeTests list)
    if e then exitSuccess else exitFailure

runTestJSON :: PutText Int -> Test -> IO Bool
runTestJSON (PutText put us0) t = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  (counts', us1) <- performTest reportStart reportError reportFail us0 t
  return $ failures counts' + errors counts' == 0
 where
  reportStart ss count = do
      -- TODO: open testcase with the top Label in `path ss`. This
      -- requires rewriting all tests with labels or somehow replacing
      -- isEqual with an Assertion I can get the preface from.
      -- The opened testcase should be successfull, with everything for
      -- a succeeding test filled in (but we can't get the actual?).
      -- putStrLn $ show $ path ss
      return count
  reportFail loc msg ss count = do
      -- replace this with modifying above this to a failure
      return $ count + 1
  reportError loc msg ss count = do
      -- TODO: set status to runtime error? perhaps in stead of count?
      writeJSON $ JSON.row "command" "push-focus"
      writeJSON $ JSON.row "command" "focus-relative"
               <> JSON.row "level" "root"
      writeJSON $ JSON.row "command" "set-properties"
               <> JSON.row "accepted" False
               <> JSON.row "status" "runtime error"
               <> JSON.row "description" "Fout tijdens uitvoering"
      writeJSON $ JSON.row "command" "pop-focus"

      writeJSON $ JSON.row "command" "new-context"
      writeJSON $ JSON.row "command" "set-properties"
               <> JSON.row "accepted" False
      writeJSON $ JSON.row "command" "new-testcase"
      writeJSON $ JSON.row "command" "set-properties"
               <> JSON.row "description" msg
               <> JSON.row "accepted" False
      return $ count + 1
