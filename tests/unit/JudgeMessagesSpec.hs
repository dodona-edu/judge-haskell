{-# LANGUAGE OverloadedStrings #-}

module JudgeMessagesSpec (spec) where

import           Data.List          (nub)
import qualified Data.Text          as T
import           System.Environment (unsetEnv)
import           Test.Hspec

import           JudgeMessages
import           Support

spec :: Spec
spec = do
  describe "translate" $ do
    it "words the statuses in Dutch" $
      map (translate Dutch) [Correct, Wrong, RuntimeError, CompilationFailed]
        `shouldBe` ["Juist antwoord", "Fout antwoord", "Fout tijdens uitvoering", "Compilatie mislukt"]

    it "words the statuses in English" $
      map (translate English) [Correct, Wrong, RuntimeError, CompilationFailed]
        `shouldBe` ["Correct", "Wrong", "Runtime error", "Compilation failed"]

    it "has a distinct, non-empty text for every message in every language" $
      mapM_ (\lang -> do
                let texts = map (translate lang) [minBound .. maxBound]
                texts `shouldNotSatisfy` any T.null
                nub texts `shouldBe` texts)
            [Dutch, English]

  describe "language" $ do
    it "reads nl as Dutch and en as English" $
      map language ["nl", "en"] `shouldBe` [Dutch, English]

    it "falls back to English for any other code" $
      map language ["fr", "NL", ""] `shouldBe` [English, English, English]

    it "reads back what languageCode writes" $
      map (language . languageCode) [Dutch, English] `shouldBe` [Dutch, English]

  describe "currentLanguage" $ do
    it "reads the language the driver set" $ do
      withLanguage "nl" currentLanguage `shouldReturn` Dutch
      withLanguage "en" currentLanguage `shouldReturn` English

    it "falls back to English for another value" $
      withLanguage "de" currentLanguage `shouldReturn` English

    it "is English when the variable is not set" $
      withLanguage "nl" (unsetEnv languageVariable >> currentLanguage) `shouldReturn` English
