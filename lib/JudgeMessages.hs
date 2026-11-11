{-# LANGUAGE OverloadedStrings #-}
-- A message without a translation in every language fails the build.
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module JudgeMessages
  ( Language (..)
  , JudgeMessage (..)
  , translate
  , language
  , languageCode
  , languageVariable
  , currentLanguage
  ) where

import           Data.Text          (Text)
import           System.Environment (lookupEnv)

data Language = Dutch | English deriving (Eq, Show)

-- | Every text the judge shows students, apart from GHC's and the test files' own.
data JudgeMessage
  = CompilationTab
  | CompilationContext
  | JudgeCompilationFailed
  | CompilationFailed
  | InterfaceNotSatisfied
  | TestCompilationFailed
  | Correct
  | Wrong
  | RuntimeError
  deriving (Eq, Show, Enum, Bounded)

translate :: Language -> JudgeMessage -> Text
translate Dutch   CompilationTab         = "compilatie"
translate English CompilationTab         = "compilation"
translate Dutch   CompilationContext     = "Compilatie"
translate English CompilationContext     = "Compilation"
translate Dutch   JudgeCompilationFailed = "Compilatie judge mislukt"
translate English JudgeCompilationFailed = "Judge compilation failed"
translate Dutch   CompilationFailed      = "Compilatie mislukt"
translate English CompilationFailed      = "Compilation failed"
translate Dutch   InterfaceNotSatisfied  = "Interface niet voldaan"
translate English InterfaceNotSatisfied  = "Interface not satisfied"
translate Dutch   TestCompilationFailed  = "Testcompilatie gefaald"
translate English TestCompilationFailed  = "Test compilation failed"
translate Dutch   Correct                = "Juist antwoord"
translate English Correct                = "Correct"
translate Dutch   Wrong                  = "Fout antwoord"
translate English Wrong                  = "Wrong"
translate Dutch   RuntimeError           = "Fout tijdens uitvoering"
translate English RuntimeError           = "Runtime error"

-- | The language for Dodona's natural_language: English for anything but nl.
language :: String -> Language
language "nl" = Dutch
language _    = English

languageCode :: Language -> String
languageCode Dutch   = "nl"
languageCode English = "en"

-- | Set by the driver for the test programs, which write their own feedback.
languageVariable :: String
languageVariable = "DODONA_NATURAL_LANGUAGE"

-- | The language from languageVariable, English when it is not set.
currentLanguage :: IO Language
currentLanguage = maybe English language <$> lookupEnv languageVariable
