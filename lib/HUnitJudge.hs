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

import           Data.Aeson (encode, Value, (.=), object)
import           Data.Aeson.Key (Key, fromText)
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified System.IO as IO
import           Test.HUnit (PutText(PutText), Assertion, Test(TestList, TestLabel), assertBool, performTest)

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

(.==) :: T.Text -> T.Text -> (Key, Value)
(.==) k = (.=) (fromText k)

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
