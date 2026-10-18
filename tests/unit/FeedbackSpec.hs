{-# LANGUAGE OverloadedStrings #-}

module FeedbackSpec (spec) where

import           Data.Aeson      (Value (..), object, toJSON, (.=))
import qualified Data.ByteString as BS
import           Test.Hspec

import           Feedback
import           Support

spec :: Spec
spec = do
  describe "toJSON" $ do
    it "writes start-judgement with only its command" $
      toJSON StartJudgement `shouldBe` command "start-judgement" []

    it "writes a hidden tab with hidden true" $
      toJSON (StartTab "compilatie" True Nothing)
        `shouldBe` command "start-tab" ["title" .= String "compilatie", "hidden" .= True]

    it "writes a visible tab without hidden" $
      toJSON (StartTab "double" False Nothing) `shouldBe` command "start-tab" ["title" .= String "double"]

    it "writes a tab's permission" $
      toJSON (StartTab "double" True (Just Staff))
        `shouldBe` command "start-tab" ["title" .= String "double", "hidden" .= True, "permission" .= String "staff"]

    it "writes a context without description as only its command" $
      toJSON (StartContext Nothing) `shouldBe` command "start-context" []

    it "writes a context's description as a message with format" $
      toJSON (StartContext (Just (Message Code "ghc Input.hs" Nothing)))
        `shouldBe` command "start-context" ["description" .= message "code" "ghc Input.hs"]

    it "writes plain as the format plain" $
      toJSON (StartContext (Just (Message Plain "Compilatie" Nothing)))
        `shouldBe` command "start-context" ["description" .= message "plain" "Compilatie"]

    it "writes a message without permission" $
      toJSON (AppendMessage (Message Code "error" Nothing))
        `shouldBe` command "append-message" ["message" .= message "code" "error"]

    it "writes a message's permission inside the message" $
      toJSON (AppendMessage (Message Code "error" (Just Staff)))
        `shouldBe` command "append-message"
          ["message" .= object ["format" .= String "code", "description" .= String "error", "permission" .= String "staff"]]

    it "writes close-context without accepted unless given" $ do
      toJSON (CloseContext Nothing) `shouldBe` command "close-context" []
      toJSON (CloseContext (Just False)) `shouldBe` command "close-context" ["accepted" .= False]

    it "writes close-tab with only its command" $
      toJSON CloseTab `shouldBe` command "close-tab" []

    it "writes close-judgement without accepted and status unless given" $
      toJSON (CloseJudgement Nothing Nothing) `shouldBe` command "close-judgement" []

    it "writes a rejected judgement with its status enum and human text" $ do
      toJSON (CloseJudgement (Just False) (Just (Status CompilationError "Compilatie mislukt")))
        `shouldBe` command "close-judgement" ["accepted" .= False, "status" .= status "compilation error" "Compilatie mislukt"]
      toJSON (CloseJudgement (Just False) (Just (Status InternalError "Testcompilatie gefaald")))
        `shouldBe` command "close-judgement" ["accepted" .= False, "status" .= status "internal error" "Testcompilatie gefaald"]

  describe "write" $ do
    it "writes commands back to back as JSON" $ do
      commands <- captureCommands (mapM_ write [StartJudgement, CloseTab])
      commands `shouldBe` [command "start-judgement" [], command "close-tab" []]

    it "writes non-ASCII text as UTF-8" $ do
      commands <- captureCommands (write (AppendMessage (Message Code "‘++’ café" Nothing)))
      fieldsOf "append-message" ["message", "description"] commands `shouldBe` ["‘++’ café"]

  describe "tabTitle" $ do
    it "is the test file's name without _test.hs, with spaces for underscores" $
      tabTitle "/mnt/resources/word_count_test.hs" `shouldBe` "word count"

    it "keeps underscores in the directory out of it" $
      tabTitle "/mnt/my_resources/double_test.hs" `shouldBe` "double"

    it "keeps the whole name of a file without _test.hs" $
      tabTitle "/mnt/resources/extra_checks.hs" `shouldBe` "extra checks.hs"

  describe "compilerOutput" $ do
    it "drops the trailing newlines" $
      compilerOutput "lib/Input.hs:3:1: error\n\n" `shouldBe` "lib/Input.hs:3:1: error"

    it "keeps the newlines inside and other trailing whitespace" $
      compilerOutput "line 1\nline 2 \n" `shouldBe` "line 1\nline 2 "

    it "decodes UTF-8" $
      compilerOutput "Perhaps use \226\128\152++\226\128\153" `shouldBe` "Perhaps use ‘++’"

    it "replaces invalid UTF-8" $
      compilerOutput (BS.pack [0x61, 0xff, 0x62]) `shouldBe` "a\xfffd\&b"

message :: String -> String -> Value
message format description = object ["format" .= format, "description" .= description]

status :: String -> String -> Value
status enum human = object ["enum" .= enum, "human" .= human]
