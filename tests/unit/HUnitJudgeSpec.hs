{-# LANGUAGE OverloadedStrings #-}

module HUnitJudgeSpec (spec) where

import           Control.Exception (AsyncException (StackOverflow), SomeException, throw, throwIO, try)
import           Data.Aeson        (Value (..), object, (.=))
import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import           System.IO
import           System.Timeout    (timeout)
import           Test.Hspec
import           Test.HUnit        (Counts (..), Node (..), Test (..), assertEqual, performTest, testCasePaths)

import           HUnitJudge
import           Support

spec :: Spec
spec = do
  describe "isEqual" $ do
    it "opens a context, testcase and test and accepts equal values" $ do
      commands <- captureCommands (isEqual "double 2" (4 :: Int) 4)
      commands `shouldBe` passing "double 2" "4" "4"

    it "rejects unequal values, taking the expected value from the first argument" $ do
      commands <- captureCommands (isEqual "double 2" (4 :: Int) 5)
      commands `shouldBe` failing "double 2" "4" "5"

    it "counts as a passed HUnit test on equal values" $ do
      counts <- countsOf (TestCase (isEqual "double 2" (4 :: Int) 4))
      counts `shouldBe` Counts {cases = 1, tried = 1, errors = 0, failures = 0}

    it "counts as a passed HUnit test on unequal values too, as it reports the wrong answer itself" $ do
      counts <- countsOf (TestCase (isEqual "double 2" (4 :: Int) 5))
      counts `shouldBe` Counts {cases = 1, tried = 1, errors = 0, failures = 0}

    it "closes the open test as a runtime error with the plain message when the submission's value throws" $ do
      commands <- captureCommands (isEqual "inc 1" (2 :: Int) (errorWithoutStackTrace "line 1\nline 2"))
      commands `shouldBe` opened "inc 1" "2" ++ runtimeError "line 1\nline 2"

    it "closes the open test as a runtime error when showing the submission's value throws" $ do
      commands <- captureCommands (isEqual "double [1]" [2 :: Int] (1 : errorWithoutStackTrace "no more"))
      commands `shouldBe` opened "double [1]" "[2]" ++ runtimeError "no more"

    it "closes the open test as a runtime error on a stack overflow in the submission's value" $ do
      commands <- captureCommands (isEqual "inc 1" (2 :: Int) (throw StackOverflow))
      commands `shouldBe` opened "inc 1" "2" ++ runtimeError "stack overflow"

    it "writes nothing and throws when the expected value throws" $ do
      (output, result) <- captureStdout (tryAny (isEqual "inc 1" (errorWithoutStackTrace "no expected" :: Int) 2))
      output `shouldBe` ""
      either (Just . show) (const Nothing) result `shouldBe` Just "no expected"

    it "has written and flushed the opened test while the submission's value is still being evaluated" $ do
      (_, (finished, written)) <- captureStdoutIn $ \path -> do
        hSetBuffering stdout (BlockBuffering Nothing)
        finished <- timeout 200000 (isEqual "length [1 ..]" (3 :: Int) (length [(1 :: Integer) ..]))
        written <- withFile path ReadMode (\h -> hSetEncoding h utf8 >> T.hGetContents h)
        pure (finished, written)
      finished `shouldBe` Nothing
      decodeCommands written `shouldBe` Right (opened "length [1 ..]" "3")

    it "rejects an infinite value against a finite expected one, cutting off what it shows" $ do
      commands <- captureCommands (isEqual "repeat 1" [1, 1 :: Int] (repeat 1))
      commands `shouldBe` failing "repeat 1" "[1,1]" (T.pack (take 10000 (show (repeat (1 :: Int)))) <> "\x2026")

    it "shows a long equal value in full" $ do
      commands <- captureCommands (isEqual "replicate 5000 1" (replicate 5000 (1 :: Int)) (replicate 5000 1))
      fieldsOf "close-test" ["generated"] commands `shouldBe` [String (T.pack (show (replicate 5000 (1 :: Int))))]

    it "shows values with show, so strings keep their quotes" $ do
      commands <- captureCommands (isEqual "reverse \"ab\"" "ba" "ba")
      commands `shouldBe` passing "reverse \"ab\"" "\"ba\"" "\"ba\""

    it "writes the description as given, non-ASCII included" $ do
      commands <- captureCommands (isEqual "lengte \"café\"" (4 :: Int) 4)
      fieldsOf "start-testcase" ["description", "description"] commands `shouldBe` ["lengte \"café\""]

  describe "makeTests" $ do
    it "lists the tests, each under an empty label" $
      testCasePaths (makeTests [TestCase (pure ()), TestCase (pure ())])
        `shouldBe` [[Label "", ListItem 0], [Label "", ListItem 1]]

    it "keeps an empty list empty" $
      testCasePaths (makeTests []) `shouldBe` []

  describe "runJSON" $ do
    it "writes only the commands of the tests for a passing list" $ do
      commands <- captureCommands $ runJSON
        [ TestCase (isEqual "inc 1" (2 :: Int) 2)
        , TestCase (isEqual "inc 2" (3 :: Int) 3) ]
      commands `shouldBe` passing "inc 1" "2" "2" ++ passing "inc 2" "3" "3"

    it "keeps running after a failing test and writes nothing extra for the failure" $ do
      commands <- captureCommands $ runJSON
        [ TestCase (isEqual "inc 1" (2 :: Int) 3)
        , TestCase (isEqual "inc 2" (3 :: Int) 3) ]
      commands `shouldBe` failing "inc 1" "2" "3" ++ passing "inc 2" "3" "3"

    it "closes the open test as a runtime error when the submission's value throws, and keeps running" $ do
      commands <- captureCommands $ runJSON
        [ TestCase (isEqual "inc 1" (2 :: Int) (errorWithoutStackTrace "line 1\nline 2"))
        , TestCase (isEqual "inc 2" (3 :: Int) 3) ]
      commands `shouldBe` opened "inc 1" "2" ++ runtimeError "line 1\nline 2" ++ passing "inc 2" "3" "3"

    it "reports the runtime error message as is" $ do
      commands <- captureCommands $ runJSON
        [TestCase (isEqual "inc 1" (2 :: Int) (errorWithoutStackTrace "line 1\nline 2"))]
      fieldsOf "close-test" ["generated"] commands `shouldBe` ["line 1\nline 2"]

    it "opens a group of its own for an error raised outside isEqual" $ do
      commands <- captureCommands (runJSON [TestCase (ioError (userError "boom"))])
      commands `shouldBe` outside "runtime error" "Fout tijdens uitvoering" "user error (boom)"

    it "opens a group of its own when the expected value throws, and keeps running" $ do
      commands <- captureCommands $ runJSON
        [ TestCase (isEqual "inc 1" (errorWithoutStackTrace "no expected" :: Int) 2)
        , TestCase (isEqual "inc 2" (3 :: Int) 3) ]
      commands `shouldBe` outside "runtime error" "Fout tijdens uitvoering" "no expected" ++ passing "inc 2" "3" "3"

    it "opens a group of its own for a failing plain HUnit assertion" $ do
      commands <- captureCommands (runJSON [TestCase (assertEqual "length" 2 (3 :: Int))])
      commands `shouldBe` outside "wrong" "Fout antwoord" "length\nexpected: 2\n but got: 3"

    it "reports an asynchronous exception HUnit rethrows, which ends the remaining tests" $ do
      commands <- captureCommands $ runJSON
        [ TestCase (isEqual "inc 1" (2 :: Int) 2)
        , TestCase (throwIO StackOverflow)
        , TestCase (isEqual "inc 2" (3 :: Int) 3) ]
      commands `shouldBe` passing "inc 1" "2" "2" ++ outside "runtime error" "Fout tijdens uitvoering" "stack overflow"

    it "writes balanced output on every path" $ do
      commands <- captureCommands $ runJSON
        [ TestCase (isEqual "correct" (1 :: Int) 1)
        , TestCase (isEqual "wrong" (1 :: Int) 2)
        , TestCase (isEqual "submission throws" (1 :: Int) (errorWithoutStackTrace "boom"))
        , TestCase (isEqual "expected throws" (errorWithoutStackTrace "boom" :: Int) 1)
        , TestCase (assertEqual "plain assertion" 1 (2 :: Int))
        , TestCase (ioError (userError "plain IO")) ]
      commands `shouldSatisfy` balanced
      length (fieldsOf "start-context" ["command"] commands) `shouldBe` 6

    it "writes nothing for an empty list" $ do
      commands <- captureCommands (runJSON [])
      commands `shouldBe` []

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

countsOf :: Test -> IO Counts
countsOf test = do
  (_, (counts, ())) <- captureStdout (performTest (\_ us -> pure us) ignore ignore () test)
  pure counts
  where
    ignore _ _ _ = pure

opened :: T.Text -> T.Text -> [Value]
opened description expected =
  [ command "start-context" []
  , command "start-testcase" ["description" .= object ["format" .= String "haskell", "description" .= description]]
  , command "start-test" ["expected" .= expected]
  ]

closed :: [Value]
closed = [command "close-testcase" [], command "close-context" []]

closeTest :: T.Text -> Bool -> T.Text -> T.Text -> Value
closeTest generated accepted enum human =
  command "close-test" [ "generated" .= generated
                       , "accepted" .= accepted
                       , "status" .= object ["enum" .= enum, "human" .= human] ]

passing :: T.Text -> T.Text -> T.Text -> [Value]
passing description expected generated =
  opened description expected ++ [closeTest generated True "correct" "Juist antwoord"] ++ closed

failing :: T.Text -> T.Text -> T.Text -> [Value]
failing description expected generated =
  opened description expected ++ [closeTest generated False "wrong" "Fout antwoord"] ++ closed

runtimeError :: T.Text -> [Value]
runtimeError generated = closeTest generated False "runtime error" "Fout tijdens uitvoering" : closed

-- | The group reportError and reportFail write for what HUnit caught outside isEqual.
outside :: T.Text -> T.Text -> T.Text -> [Value]
outside enum human message =
  [ command "start-context" []
  , command "start-testcase" ["description" .= object ["format" .= String "plain", "description" .= human]]
  , command "append-message" ["message" .= object ["format" .= String "code", "description" .= message]]
  , command "close-testcase" ["accepted" .= False]
  , command "escalate-status" ["status" .= object ["enum" .= enum, "human" .= human]]
  , command "close-context" []
  ]
