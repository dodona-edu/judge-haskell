{-# LANGUAGE OverloadedStrings #-}

module QuickCheckJudgeSpec (spec) where

import           Data.Aeson      (Value (..), object, (.=))
import           Data.Maybe      (isNothing)
import qualified Data.Text       as T
import           Test.Hspec
import           Test.QuickCheck (Args (..), quickCheckWithResult)

import qualified HUnitJudge
import           QuickCheckJudge
import           Support

spec :: Spec
spec = do
  describe "judgeArgs" $ do
    it "uses QuickCheck's defaults" $ do
      replay judgeArgs `shouldSatisfy` isNothing
      maxSuccess judgeArgs `shouldBe` 100
      maxDiscardRatio judgeArgs `shouldBe` 10
      maxSize judgeArgs `shouldBe` 100
      maxShrinks judgeArgs `shouldBe` 100

    it "is not chatty" $
      chatty judgeArgs `shouldBe` False

    it "keeps QuickCheck silent on stdout" $ do
      (output, _) <- captureStdout (quickCheckWithResult judgeArgs alwaysFalse)
      output `shouldBe` ""

  describe "checkResult" $ do
    it "accepts a property that holds, with QuickCheck's report as message" $ do
      result <- checkResult <$> quickCheckWithResult judgeArgs keepsLength
      accepted result `shouldBe` True
      message result `shouldBe` "+++ OK, passed 100 tests.\n"

    it "rejects a property that fails, with the counterexample in the message" $ do
      result <- checkResult <$> quickCheckWithResult judgeArgs alwaysFalse
      accepted result `shouldBe` False
      message result `shouldBe` "*** Failed! Falsified (after 1 test):\n()\n"

  describe "executeTestCase" $ do
    it "writes a context with an accepted testcase and a correct status for a property that holds" $ do
      commands <- captureCommands (executeTestCase (TestCase "reverse keeps the length" keepsLength))
      commands `shouldBe` testCase "reverse keeps the length" "+++ OK, passed 100 tests.\n" True "Correct" "correct"

    it "writes a context with a rejected testcase and a wrong status for a property that fails" $ do
      commands <- captureCommands (executeTestCase (TestCase "always false" alwaysFalse))
      commands `shouldBe` testCase "always false" "*** Failed! Falsified (after 1 test):\n()\n" False "Wrong" "wrong"

    it "words a correct status as HUnitJudge does" $ do
      quickCheck <- captureCommands (executeTestCase (TestCase "reverse keeps the length" keepsLength))
      hunit <- captureCommands (HUnitJudge.isEqual "reverse keeps the length" True True)
      statusOf quickCheck `shouldBe` statusOf hunit

    it "words a wrong status as HUnitJudge does" $ do
      quickCheck <- captureCommands (executeTestCase (TestCase "always false" alwaysFalse))
      hunit <- captureCommands (ignoringFailure (HUnitJudge.isEqual "always false" True False))
      statusOf quickCheck `shouldBe` statusOf hunit

    it "words the statuses in Dutch when the driver sets nl" $ do
      correct <- withLanguage "nl" (captureCommands (executeTestCase (TestCase "reverse keeps the length" keepsLength)))
      wrong <- withLanguage "nl" (captureCommands (executeTestCase (TestCase "always false" alwaysFalse)))
      fieldsOf "escalate-status" ["status", "human"] (correct ++ wrong) `shouldBe` ["Juist antwoord", "Fout antwoord"]

keepsLength :: [Int] -> Bool
keepsLength xs = length (reverse xs) == length xs

-- | Fails on the first try without shrinking, so QuickCheck's report is deterministic.
alwaysFalse :: () -> Bool
alwaysFalse = const False

testCase :: T.Text -> T.Text -> Bool -> T.Text -> T.Text -> [Value]
testCase description report isAccepted human enum =
  [ command "start-context" []
  , command "start-testcase" ["description" .= object ["format" .= String "haskell", "description" .= description]]
  , command "append-message" ["message" .= object ["format" .= String "code", "description" .= report]]
  , command "close-testcase" ["accepted" .= isAccepted]
  , command "escalate-status" ["status" .= object ["human" .= human, "enum" .= enum]]
  , command "close-context" []
  ]
