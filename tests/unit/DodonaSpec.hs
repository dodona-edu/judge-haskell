{-# LANGUAGE OverloadedStrings #-}

module DodonaSpec (spec) where

import           Data.Aeson       (Value (..), eitherDecodeStrictText, object, (.=))
import qualified Data.Text        as T
import           System.Exit      (ExitCode (..))
import           System.Process   (proc, readCreateProcessWithExitCode)
import           Test.Hspec

spec :: Spec
spec = describe "close-test" $ do
  it "writes the test as one JSON object when given generated, enum, human and accepted" $ do
    (code, output) <- runDodona ["close-test", "-g", "42", "-e", "correct", "-h", "ok", "-a"]
    code `shouldBe` ExitSuccess
    parsed <- either fail pure (eitherDecodeStrictText (T.pack output))
    parsed `shouldBe` object
      [ "command" .= String "close-test"
      , "generated" .= String "42"
      , "accepted" .= Bool True
      , "status" .= object ["enum" .= String "correct", "human" .= String "ok"]
      ]

  it "exits with an error when enum and human are missing" $ do
    (code, _) <- runDodona ["close-test", "-g", "42", "-a"]
    code `shouldSatisfy` (/= ExitSuccess)

-- | Runs the dodona script from the repository root, where the unit tests run.
runDodona :: [String] -> IO (ExitCode, String)
runDodona args = do
  (code, output, _) <- readCreateProcessWithExitCode (proc "./dodona" args) ""
  pure (code, output)
