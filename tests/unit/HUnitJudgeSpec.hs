{-# LANGUAGE OverloadedStrings #-}

module HUnitJudgeSpec (spec) where

import           Data.Aeson        (Value (..), object, (.=))
import qualified Data.Text         as T
import           System.IO
import           Test.Hspec
import           Test.HUnit        (Counts (..), Node (..), Test (..), performTest, testCasePaths)
import           Test.HUnit.Lang   (HUnitFailure)

import           HUnitJudge
import           Support

spec :: Spec
spec = do
  describe "isEqual" $ do
    it "opens a context, testcase and test and accepts equal values" $ do
      commands <- captureCommands (isEqual "double 2" (4 :: Int) 4)
      commands `shouldBe` passing "double 2" "4" "4"

    it "rejects unequal values, taking the expected value from the first argument" $ do
      commands <- captureCommands (ignoringFailure (isEqual "double 2" (4 :: Int) 5))
      commands `shouldBe` failing "double 2" "4" "5"

    it "throws an HUnit failure on unequal values" $
      captureStdout (isEqual "double 2" (4 :: Int) 5) `shouldThrow` anyHUnitFailure

    it "counts as a passed HUnit test on equal values" $ do
      counts <- countsOf (TestCase (isEqual "double 2" (4 :: Int) 4))
      counts `shouldBe` Counts {cases = 1, tried = 1, errors = 0, failures = 0}

    it "counts as one HUnit failure on unequal values" $ do
      counts <- countsOf (TestCase (isEqual "double 2" (4 :: Int) 5))
      counts `shouldBe` Counts {cases = 1, tried = 1, errors = 0, failures = 1}

    it "shows values with show, so strings keep their quotes" $ do
      commands <- captureCommands (isEqual "reverse \"ab\"" "ba" "ba")
      commands `shouldBe` passing "reverse \"ab\"" "\"ba\"" "\"ba\""

    it "words the statuses in Dutch when the driver sets nl" $ do
      correct <- withLanguage "nl" (captureCommands (isEqual "double 2" (4 :: Int) 4))
      wrong <- withLanguage "nl" (captureCommands (ignoringFailure (isEqual "double 2" (4 :: Int) 5)))
      fieldsOf "close-test" ["status", "human"] (correct ++ wrong) `shouldBe` ["Juist antwoord", "Fout antwoord"]

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
      commands `shouldBe` opened "inc 1" "2" ++ runtimeError "\"line 1\\nline 2\"" ++ passing "inc 2" "3" "3"

    it "reports the runtime error message as is" $ do
      commands <- captureCommands $ runJSON
        [TestCase (isEqual "inc 1" (2 :: Int) (errorWithoutStackTrace "line 1\nline 2"))]
      knownBug "#27 runtime error text is shown with show" $
        fieldsOf "close-test" ["generated"] commands `shouldBe` ["line 1\nline 2"]

    it "closes a test, testcase and context it never opened when the error is raised outside isEqual" $ do
      commands <- captureCommands (runJSON [TestCase (ioError (userError "boom"))])
      commands `shouldBe` runtimeError "\"user error (boom)\""

    it "words the runtime error in Dutch when the driver sets nl" $ do
      commands <- withLanguage "nl" $ captureCommands $ runJSON [TestCase (ioError (userError "boom"))]
      fieldsOf "close-test" ["status", "human"] commands `shouldBe` ["Fout tijdens uitvoering"]

    it "writes nothing for an empty list" $ do
      commands <- captureCommands (runJSON [])
      commands `shouldBe` []

    it "line-buffers stdout" $ do
      (_, buffering) <- captureStdout $ do
        hSetBuffering stdout NoBuffering
        runJSON []
        hGetBuffering stdout
      buffering `shouldBe` LineBuffering

anyHUnitFailure :: Selector HUnitFailure
anyHUnitFailure = const True

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
  opened description expected ++ [closeTest generated True "correct" "Correct"] ++ closed

failing :: T.Text -> T.Text -> T.Text -> [Value]
failing description expected generated =
  opened description expected ++ [closeTest generated False "wrong" "Wrong"] ++ closed

runtimeError :: T.Text -> [Value]
runtimeError generated = closeTest generated False "runtime error" "Runtime error" : closed
