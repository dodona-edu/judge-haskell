{-# LANGUAGE OverloadedStrings #-}

module UtilsSpec (spec) where

import           Data.Aeson (Value (..))
import           Data.Maybe (fromMaybe)
import           Test.Hspec
import           Test.HUnit (Test)

import           HUnitJudge (runJSON)
import           Support
import           Utils

spec :: Spec
spec = do
  describe "sjoin" $ do
    it "joins words with single spaces" $
      sjoin ["half", "4", "(-3)"] `shouldBe` "half 4 (-3)"

    it "keeps the words' own spaces" $
      sjoin ["a b", "c"] `shouldBe` "a b c"

    it "returns a single word unchanged" $
      sjoin ["half"] `shouldBe` "half"

    it "returns the empty string for no words" $
      sjoin [] `shouldBe` ""

  describe "generateTest1" $ do
    it "makes one test per input" $
      length (generateTest1 "double" double double [1, 2, 3]) `shouldBe` 3

    it "describes each test as the function applied to the shown argument" $ do
      commands <- run (generateTest1 "double" double double [1, 2, 3])
      descriptions commands `shouldBe` ["double 1", "double 2", "double 3"]

    it "expects the first function's result and generates the second's" $ do
      commands <- run (generateTest1 "double" double (+ 2) [1, 2, 3])
      expected commands `shouldBe` ["2", "4", "6"]
      generated commands `shouldBe` ["3", "4", "5"]
      acceptance commands `shouldBe` [False, True, False]

    it "shows string arguments with quotes" $ do
      commands <- run (generateTest1 "length" length length ["ab" :: String])
      descriptions commands `shouldBe` ["length \"ab\""]

    it "makes no tests for no inputs" $
      length (generateTest1 "double" double double []) `shouldBe` 0

    it "puts a negative argument between parentheses" $ do
      commands <- run (generateTest1 "half" half half [-3])
      descriptions commands `shouldBe` ["half (-3)"]

    it "puts a constructor application argument between parentheses" $ do
      commands <- run (generateTest1 "fromMaybe0" fromMaybe0 fromMaybe0 [Just 3])
      descriptions commands `shouldBe` ["fromMaybe0 (Just 3)"]

    it "leaves a positive argument without parentheses" $ do
      commands <- run (generateTest1 "half" half half [4])
      descriptions commands `shouldBe` ["half 4"]

  describe "generateTest2" $
    it "describes and runs one test per pair" $ do
      commands <- run (generateTest2 "plus" plus plus [(1, 2), (3, 4)])
      descriptions commands `shouldBe` ["plus 1 2", "plus 3 4"]
      generated commands `shouldBe` ["3", "7"]
      acceptance commands `shouldBe` [True, True]

  describe "generateTest3" $
    it "describes and runs one test per triple" $ do
      commands <- run (generateTest3 "sum3" sum3 sum3 [(1, 2, 3), (4, 5, 6)])
      descriptions commands `shouldBe` ["sum3 1 2 3", "sum3 4 5 6"]
      generated commands `shouldBe` ["6", "15"]

  describe "generateTest4" $
    it "describes and runs one test per quadruple" $ do
      commands <- run (generateTest4 "sum4" sum4 sum4 [(1, 2, 3, 4)])
      descriptions commands `shouldBe` ["sum4 1 2 3 4"]
      generated commands `shouldBe` ["10"]

  describe "generateInfix" $ do
    it "describes each test as the operator between the shown arguments" $ do
      commands <- run (generateInfix "`div`" div' div' [(7, 2), (9, 3)])
      descriptions commands `shouldBe` ["7 `div` 2", "9 `div` 3"]
      generated commands `shouldBe` ["3", "3"]

    it "puts negative operands between parentheses" $ do
      commands <- run (generateInfix "`div`" div' div' [(-7, -2)])
      descriptions commands `shouldBe` ["(-7) `div` (-2)"]

    it "expects the first function's result and generates the second's" $ do
      commands <- run (generateInfix "-" minus plus [(5, 2)])
      expected commands `shouldBe` ["3"]
      generated commands `shouldBe` ["7"]
      acceptance commands `shouldBe` [False]

  describe "combos2" $ do
    it "pairs every element of the first list with every element of the second, in order" $
      combos2 [1, 2 :: Int] "ab" `shouldBe` [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]

    it "is empty when a list is empty" $
      combos2 [1, 2 :: Int] "" `shouldBe` []

  describe "combos3" $
    it "combines every element of three lists, the last varying fastest" $
      combos3 [1, 2 :: Int] "ab" [True] `shouldBe`
        [(1, 'a', True), (1, 'b', True), (2, 'a', True), (2, 'b', True)]

  describe "combos4" $ do
    it "combines every element of four lists, the last varying fastest" $
      combos4 [1 :: Int] "ab" [True] [LT, GT] `shouldBe`
        [(1, 'a', True, LT), (1, 'a', True, GT), (1, 'b', True, LT), (1, 'b', True, GT)]

    it "has as many combinations as the product of the lengths" $
      length (combos4 [1, 2 :: Int] "abc" [True, False] [(), ()]) `shouldBe` 24

run :: [Test] -> IO [Value]
run = captureCommands . runJSON

descriptions :: [Value] -> [Value]
descriptions = fieldsOf "start-testcase" ["description", "description"]

expected :: [Value] -> [Value]
expected = fieldsOf "start-test" ["expected"]

generated :: [Value] -> [Value]
generated = fieldsOf "close-test" ["generated"]

acceptance :: [Value] -> [Bool]
acceptance commands = [accepted | Bool accepted <- fieldsOf "close-test" ["accepted"] commands]

double :: Int -> Int
double = (* 2)

half :: Int -> Int
half = (`div` 2)

fromMaybe0 :: Maybe Int -> Int
fromMaybe0 = fromMaybe 0

plus, minus, div' :: Int -> Int -> Int
plus = (+)
minus = (-)
div' = div

sum3 :: Int -> Int -> Int -> Int
sum3 a b c = a + b + c

sum4 :: Int -> Int -> Int -> Int -> Int
sum4 a b c d = a + b + c + d
