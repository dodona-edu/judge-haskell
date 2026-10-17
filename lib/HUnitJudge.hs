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

import           Control.Exception (AsyncException(StackOverflow, HeapOverflow), SomeAsyncException, SomeException, evaluate, fromException, try, tryJust)
import           Data.Aeson (Value, (.=), object)
import           Data.Aeson.Key (Key, fromText)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.List (intercalate)
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified System.IO as IO
import           Test.HUnit (PutText(PutText), Assertion, Test(TestList, TestLabel), performTest)

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
-- Encoded in full, then flushed: Dodona rejects all output after a cut-off object, and keeps what was written on a timeout.
--
writeJSON :: Value -> IO ()
writeJSON value = do
    T.putStr $! TL.toStrict (encodeToLazyText value)
    IO.hFlush IO.stdout

(.==) :: T.Text -> T.Text -> (Key, Value)
(.==) k = (.=) (fromText k)

status :: T.Text -> T.Text -> Value
status enum human = object [ "enum" .= enum, "human" .= human ]

isEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
isEqual preface should actual = do
    expected <- evaluate (T.pack (show should))
    writeJSON $ object [ "command" .== "start-context" ]
    writeJSON $ object [ "command" .== "start-testcase"
                       , "description" .= object [ "format" .== "haskell", "description" .= preface ] ]
    writeJSON $ object [ "command" .== "start-test"
                       , "expected" .= expected ]
    result <- tryJust studentError $ do
        accepted <- evaluate (should == actual)
        generated <- evaluate (T.pack (if accepted then show actual else truncated (show actual)))
        return (accepted, generated)
    case result of
        Right (True, generated)  -> closeTest generated True (status "correct" "Juist antwoord")
        Right (False, generated) -> closeTest generated False (status "wrong" "Fout antwoord")
        Left message             -> closeTest (T.pack message) False (status "runtime error" "Fout tijdens uitvoering")
    writeJSON $ object [ "command" .== "close-testcase" ]
    writeJSON $ object [ "command" .== "close-context" ]
  where
    closeTest generated accepted testStatus =
        writeJSON $ object [ "command" .== "close-test"
                           , "generated" .= generated
                           , "accepted" .= accepted
                           , "status" .= testStatus ]

-- Overflows are the submission's doing; other asynchronous exceptions (timeouts, kills) must pass.
studentError :: SomeException -> Maybe String
studentError e
    | isOverflow || not isAsync = Just (show e)
    | otherwise                 = Nothing
  where
    isOverflow = fromException e `elem` map Just [StackOverflow, HeapOverflow]
    isAsync = isJust (fromException e :: Maybe SomeAsyncException)

-- Keeps an infinite generated value, compared against a finite expected one, from hanging the judge.
truncated :: String -> String
truncated text = case splitAt 10000 text of
    (shown, []) -> shown
    (shown, _)  -> shown ++ "\x2026"

makeTests :: [Test] -> Test
makeTests list = TestList $ map (TestLabel "")   list

runJSON :: [Test] -> IO ()
runJSON list = do
    -- TODO: improve focus so I can set the tab status in here,
    -- something like {"command": "focus", "level": "tab"}
    runTestJSON myPutText (makeTests list)

runTestJSON :: PutText () -> Test -> IO ()
runTestJSON (PutText put us0) t = do
  -- HUnit rethrows asynchronous exceptions, which ends the remaining tests.
  result <- try (performTest reportStart reportError reportFail us0 t)
  case result of
    Left e  -> reportOutside "runtime error" "Fout tijdens uitvoering" (show (e :: SomeException))
    Right _ -> return ()
 where
  reportStart ss count = return ()
      -- TODO: open testcase with the top Label in `path ss`. This
      -- requires rewriting all tests with labels or somehow replacing
      -- isEqual with an Assertion I can get the preface from.
      -- The opened testcase should be successfull, with everything for
      -- a succeeding test filled in (but we can't get the actual?).
      -- putStrLn $ show $ path ss
  reportFail loc msg ss count = reportOutside "wrong" "Fout antwoord" msg
  reportError loc msg ss count = reportOutside "runtime error" "Fout tijdens uitvoering" msg

-- HUnit only sees failures from outside isEqual, which has nothing open for them.
reportOutside :: T.Text -> T.Text -> String -> IO ()
reportOutside enum human message = mapM_ writeJSON
    [ object [ "command" .== "start-context" ]
    , object [ "command" .== "start-testcase"
             , "description" .= object [ "format" .== "plain", "description" .= human ] ]
    , object [ "command" .== "append-message"
             , "message" .= object [ "format" .== "code", "description" .= message ] ]
    , object [ "command" .== "close-testcase", "accepted" .= False ]
    , object [ "command" .== "escalate-status", "status" .= status enum human ]
    , object [ "command" .== "close-context" ]
    ]
