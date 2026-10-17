{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import           Data.Either (isLeft)
import           Test.Hspec

import           Config

spec :: Spec
spec = do
  describe "parseConfig" $ do
    it "reads the paths from Dodona's config and ignores the other keys" $
      parseConfig "{\"time_limit\": 30, \"memory_limit\": 536870912, \"natural_language\": \"nl\",\
                  \ \"programming_language\": \"haskell\", \"judge\": \"/mnt/judge\",\
                  \ \"resources\": \"/mnt/resources\", \"source\": \"/mnt/submission/source\",\
                  \ \"workdir\": \"/home/runner/workdir\"}"
        `shouldBe` Right Config { resources = "/mnt/resources", judge = "/mnt/judge"
                                , workdir = "/home/runner/workdir", source = "/mnt/submission/source" }

    it "fails when a path is missing" $
      parseConfig "{\"judge\": \"/mnt/judge\", \"resources\": \"/mnt/resources\", \"workdir\": \"/w\"}"
        `shouldSatisfy` isLeft

    it "fails on anything but a JSON object" $
      parseConfig "" `shouldSatisfy` isLeft

  describe "orderedTests" $ do
    it "prefixes each line with the resources directory, in the file's order" $
      orderedTests "/r" "word_count_test.hs\nlongest_test.hs\n" `shouldBe` ["/r/word_count_test.hs", "/r/longest_test.hs"]

    it "reads a last line without newline" $
      orderedTests "/r" "a_test.hs\nb_test.hs" `shouldBe` ["/r/a_test.hs", "/r/b_test.hs"]

    it "turns an empty line into the resources directory itself" $
      orderedTests "/r" "a_test.hs\n\nb_test.hs\n" `shouldBe` ["/r/a_test.hs", "/r/", "/r/b_test.hs"]

    it "splits a line on spaces and tabs, prefixing only its first word" $
      orderedTests "/r" "a_test.hs b_test.hs\tc_test.hs\n" `shouldBe` ["/r/a_test.hs", "b_test.hs", "c_test.hs"]

    it "keeps a carriage return as part of the name" $
      orderedTests "/r" "a_test.hs\r\n" `shouldBe` ["/r/a_test.hs\r"]

    it "prefixes an absolute path too" $
      orderedTests "/r" "/a_test.hs\n" `shouldBe` ["/r//a_test.hs"]

  describe "globbedTests" $ do
    it "keeps the *_test.hs names, sorted by code point, under the resources directory" $
      globbedTests "/r" ["b_test.hs", "Helper.hs", "a_test.hs", "typecheck.hs", "Z_test.hs", "ordered_tests"]
        `shouldBe` ["/r/Z_test.hs", "/r/a_test.hs", "/r/b_test.hs"]

    it "leaves out hidden files" $
      globbedTests "/r" [".a_test.hs", "b_test.hs"] `shouldBe` ["/r/b_test.hs"]

    it "is empty without test files" $
      globbedTests "/r" ["Helper.hs"] `shouldBe` []

  describe "isResourceModule" $ do
    it "accepts a Haskell module" $
      isResourceModule "Helper.hs" `shouldBe` True

    it "rejects test files and typecheck.hs" $ do
      isResourceModule "double_test.hs" `shouldBe` False
      isResourceModule "typecheck.hs" `shouldBe` False

    it "rejects other files" $ do
      isResourceModule "ordered_tests" `shouldBe` False
      isResourceModule "words.txt" `shouldBe` False

    it "matches names case-sensitively" $
      isResourceModule "Typecheck.hs" `shouldBe` True
