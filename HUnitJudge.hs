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

{-# LANGUAGE OverloadedStrings #-}

module HUnitJudge where

import Test.HUnit
import Data.List (intercalate)
import Data.Aeson (encode, Value, (.=), object)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text as T
--import qualified Data.ByteString.Lazy as BL
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
writeJSON :: Value -> IO ()
-- writeJSON = BL.putStr . encode
writeJSON = T.putStr . T.decodeUtf8 . encode

(.==) :: T.Text -> T.Text -> (T.Text, Value)
(.==) = (.=)

isEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
isEqual preface should actual = do
    writeJSON $ object [ "command" .== "start-context" ]
    writeJSON $ object [ "command" .== "start-testcase"
                       , "description" .= object [ "format" .== "haskell", "description" .= preface ] ]
    writeJSON $ object [ "command" .== "start-test"
                       , "expected" .= show should ]
    let accepted = should == actual
    if accepted then writeJSON $ object [ "command" .== "close-test"
                                        , "generated" .= (show actual)
                                        , "accepted" .= True
                                        , "status" .= object [ "enum" .== "correct", "human" .== "Juist antwoord" ] ]
                else writeJSON $ object [ "command" .== "close-test"
                                        , "generated" .= (show actual)
                                        , "accepted" .= False
                                        , "status" .= object [ "enum" .== "wrong", "human" .== "Fout antwoord" ] ]
    writeJSON $ object [ "command" .== "close-testcase" ]
    writeJSON $ object [ "command" .== "close-context" ]
    assertBool "" accepted

---
--- Helper function to join strings with a space
---
s_concat :: [String] -> String
s_concat =  intercalate " "

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
      writeJSON $ object [ "command" .== "close-test"
                         , "accepted" .= False
                         , "status" .= object [ "enum" .== "runtime error", "human" .== "Fout tijdens uitvoering" ]
                         , "generated" .= (show msg) ]
      writeJSON $ object [ "command" .== "close-testcase" ]
      writeJSON $ object [ "command" .== "close-context" ]
