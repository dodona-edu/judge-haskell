{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..)
  , parseConfig
  , testFiles
  , orderedTests
  , globbedTests
  , isResourceModule
  ) where

import           Data.Aeson       (FromJSON (..), eitherDecodeStrict, withObject, (.:))
import qualified Data.ByteString  as BS
import           Data.List        (isPrefixOf, isSuffixOf, sort)
import           System.Directory (doesFileExist, listDirectory)
import           System.FilePath  ((</>))

-- | The keys the judge uses from the submission config Dodona sends on stdin.
data Config = Config
  { resources :: FilePath
  , judge     :: FilePath
  , workdir   :: FilePath
  , source    :: FilePath
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "config" $ \o ->
    Config <$> o .: "resources" <*> o .: "judge" <*> o .: "workdir" <*> o .: "source"

parseConfig :: BS.ByteString -> Either String Config
parseConfig = eitherDecodeStrict

-- | The test files to run, in order; paths that are no file are left for the caller to skip.
testFiles :: FilePath -> IO [FilePath]
testFiles dir = do
  let order = dir </> "ordered_tests"
  ordered <- doesFileExist order
  if ordered
    then orderedTests dir <$> readFile order
    else globbedTests dir <$> listDirectory dir

-- | Each line of ordered_tests prefixed with the resources directory, then split as the shell splits words.
orderedTests :: FilePath -> String -> [FilePath]
orderedTests dir = concatMap (shellWords . ((dir ++ "/") ++)) . lines

-- | The *_test.hs files among the names, in the order of the shell's glob.
globbedTests :: FilePath -> [FilePath] -> [FilePath]
globbedTests dir names =
  [ dir </> name | name <- sort names, "_test.hs" `isSuffixOf` name, not ("." `isPrefixOf` name) ]

-- | Whether a file in the resources is a module to compile with the submission.
isResourceModule :: FilePath -> Bool
isResourceModule name =
  ".hs" `isSuffixOf` name && not ("_test.hs" `isSuffixOf` name) && name /= "typecheck.hs"

-- | Splits on the shell's default IFS: spaces, tabs and newlines.
shellWords :: String -> [String]
shellWords text = case dropWhile isSeparator text of
  ""   -> []
  rest -> let (word, more) = break isSeparator rest in word : shellWords more
  where isSeparator = (`elem` [' ', '\t', '\n'])
