{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Support
  ( captureStdout
  , captureStdoutIn
  , captureCommands
  , balanced
  , command
  , decodeCommands
  , knownBug
  , field
  , fieldsOf
  , statusOf
  ) where

import           Control.Exception        (finally, try)
import           Data.Aeson               (Value (..), eitherDecodeStrictText, object, (.=))
import qualified Data.Aeson.Key           as Key
import qualified Data.Aeson.KeyMap        as KeyMap
import           Data.Aeson.Types         (Pair)
import           Data.Char                (isSpace)
import           Data.Maybe               (mapMaybe)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           GHC.IO.Handle            (hDuplicate, hDuplicateTo)
import           System.Directory         (getTemporaryDirectory, removeFile)
import           System.IO
import           Test.Hspec
import           Test.HUnit.Lang          (HUnitFailure)

-- | Runs the action with stdout redirected to a UTF-8 temp file, as the image's C.UTF-8 locale would.
captureStdout :: IO a -> IO (T.Text, a)
captureStdout = captureStdoutIn . const

-- | Like captureStdout, passing the action the file's path, to read what reached it before stdout is flushed.
captureStdoutIn :: (FilePath -> IO a) -> IO (T.Text, a)
captureStdoutIn action = do
  tmp <- getTemporaryDirectory
  (path, file) <- openTempFile tmp "stdout"
  hSetEncoding file utf8
  hFlush stdout
  saved <- hDuplicate stdout
  buffering <- hGetBuffering stdout
  hDuplicateTo file stdout
  hClose file
  result <- action path `finally` restore saved buffering
  output <- withFile path ReadMode (\h -> hSetEncoding h utf8 >> T.hGetContents h)
  removeFile path
  pure (output, result)
  where
    restore saved buffering = do
      hFlush stdout
      hDuplicateTo saved stdout
      hSetBuffering stdout buffering
      hClose saved

captureCommands :: IO a -> IO [Value]
captureCommands action = do
  (output, _) <- captureStdout action
  either fail pure (decodeCommands output)

command :: T.Text -> [Pair] -> Value
command name pairs = object (("command" .= name) : pairs)

-- | Decodes JSON objects written back to back, the way the judge emits them.
decodeCommands :: T.Text -> Either String [Value]
decodeCommands = traverse (eitherDecodeStrictText . T.pack) . splitObjects . T.unpack

splitObjects :: String -> [String]
splitObjects text = case dropWhile isSpace text of
  ""   -> []
  rest -> let (object, more) = takeObject rest in object : splitObjects more

takeObject :: String -> (String, String)
takeObject = go (0 :: Int) False ""
  where
    go depth True acc ('\\' : c : cs) = go depth True (c : '\\' : acc) cs
    go depth True acc (c : cs)        = go depth (c /= '"') (c : acc) cs
    go depth False acc (c : cs)
      | c == '"'               = go depth True (c : acc) cs
      | c == '{'               = go (depth + 1) False (c : acc) cs
      | c == '}' && depth == 1 = (reverse (c : acc), cs)
      | c == '}'               = go (depth - 1) False (c : acc) cs
      | otherwise              = go depth False (c : acc) cs
    go _ _ acc []                     = (reverse acc, [])

-- | Whether every start-… command is closed by the matching close-…, innermost first, as Dodona requires.
balanced :: [Value] -> Bool
balanced = go []
  where
    go open (c : cs) = case T.breakOn "-" <$> commandName c of
      Just ("start", level) -> go (level : open) cs
      Just ("close", level) -> case open of
        top : rest | top == level -> go rest cs
        _                         -> False
      _ -> go open cs
    go open [] = null open
    commandName c = case field ["command"] c of
      Just (String name) -> Just name
      _                  -> Nothing

-- | Asserts the intended behaviour for a filed bug: pending while it fails, a failure once it holds.
knownBug :: String -> Expectation -> Expectation
knownBug bug expectation = do
  result <- try expectation
  case result of
    Left (_ :: HUnitFailure) -> pendingWith ("known bug " ++ bug)
    Right ()                 -> expectationFailure ("looks fixed, remove knownBug: " ++ bug)

field :: [T.Text] -> Value -> Maybe Value
field [] value                 = Just value
field (k : ks) (Object object) = KeyMap.lookup (Key.fromText k) object >>= field ks
field _ _                      = Nothing

fieldsOf :: T.Text -> [T.Text] -> [Value] -> [Value]
fieldsOf name path = mapMaybe select
  where
    select command
      | field ["command"] command == Just (String name) = field path command
      | otherwise                                         = Nothing

statusOf :: [Value] -> [Value]
statusOf = mapMaybe (field ["status"])
