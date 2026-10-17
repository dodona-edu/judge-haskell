{-# LANGUAGE OverloadedStrings #-}

module JSONLinterSpec (spec) where

import           Control.Exception (finally)
import           Data.Aeson        (Value (..), object, (.=))
import qualified Data.Text         as T
import           System.Directory  (createDirectory, getTemporaryDirectory, removeDirectoryRecursive,
                                    removeFile)
import           System.Environment (getEnv)
import           System.Exit       (ExitCode (..))
import           System.IO         (hClose, openTempFile)
import           System.Process    (CreateProcess (..), proc, readCreateProcessWithExitCode)
import           Test.Hspec

import           Support

spec :: Spec
spec = do
  describe "on a submission" $ do
    it "writes nothing for code without hints" $
      lint "double :: Int -> Int\ndouble x = x * 2\n" `shouldReturn` (ExitSuccess, [])

    it "annotates a hint at its zero-based row in the submission and zero-based column" $ do
      (_, annotations) <- lint "double :: Int -> Int\ndouble x = x * 2\nf x = (x)\n"
      fieldsOf "annotate-code" ["row"] annotations `shouldBe` [Number 2]
      fieldsOf "annotate-code" ["column"] annotations `shouldBe` [Number 6]

    it "writes one annotate-code command per hint and exits successfully" $
      lint "f x = (x)\n" `shouldReturn`
        (ExitSuccess, [annotation "warning" (0, 6) (1, 3) "Redundant bracket. Why not: x"])

    it "marks one row and the width of a single-line hint" $ do
      (_, annotations) <- lint "f x = (x)\n"
      map (\a -> (field ["rows"] a, field ["columns"] a)) annotations `shouldBe` [(Just (Number 1), Just (Number 3))]

    it "marks the rows of a multi-line hint, and the last row's columns from the row's start" $ do
      (_, annotations) <- lint "h xs = concat\n  (map\n    show xs)\n"
      annotations `shouldBe` [annotation "warning" (0, 7) (3, 12) "Use concatMap. Why not: concatMap show xs"]

    it "annotates every hint, in source order" $ do
      (_, annotations) <- lint "g xs = concat (map show xs)\nn x = x == True\n"
      annotations `shouldBe`
        [ annotation "warning" (0, 7) (1, 20) "Use concatMap. Why not: concatMap show xs"
        , annotation "warning" (1, 6) (1, 9) "Redundant ==. Why not: x"
        ]

    it "maps hlint's suggestions to info" $ do
      (_, annotations) <- lint "my_value = 3\n"
      annotations `shouldBe` [annotation "info" (0, 0) (1, 12) "Use camelCase. Why not: myValue = ..."]

    it "maps hlint's warnings to warning" $ do
      (_, annotations) <- lint "f x = (x)\n"
      fieldsOf "annotate-code" ["type"] annotations `shouldBe` ["warning"]

    it "maps a parse error to error, without a suggested replacement" $ do
      (_, annotations) <- lint "f x = = 3\n"
      annotations `shouldBe` [annotation "error" (0, 6) (1, 1) "Parse error: on input `='"]

    it "annotates a hint at its row in a submission with its own module header" $ do
      (_, annotations) <- lint "module Double where\n\nf x = (x)\n"
      annotations `shouldBe` [annotation "warning" (2, 6) (1, 3) "Redundant bracket. Why not: x"]

    it "annotates a hint at its row in a submission with a LANGUAGE pragma" $ do
      (_, annotations) <- lint "{-# LANGUAGE LambdaCase #-}\nf = \\case\n  x -> (x)\n"
      annotations `shouldBe` [annotation "warning" (2, 7) (1, 3) "Redundant bracket. Why not: x"]

  describe "on its arguments" $ do
    it "fails without writing anything when no file is given" $
      runLinter [] `shouldReturn` (ExitFailure 1, "")

    it "fails without writing anything when more than one file is given" $
      runLinter ["Input.hs", "Other.hs"] `shouldReturn` (ExitFailure 1, "")

-- | An annotation from its type, zero-based (row, column), number of (rows, columns) and text.
annotation :: T.Text -> (Int, Int) -> (Int, Int) -> T.Text -> Value
annotation kind (row, column) (rows, columns) text =
  object [ "command" .= String "annotate-code", "type" .= kind
         , "row" .= row, "column" .= column, "rows" .= rows, "columns" .= columns
         , "text" .= text ]

-- | Lints a submission the way run does: as written, in a file named like Dodona's submission file.
lint :: String -> IO (ExitCode, [Value])
lint submission = withTempDirectory $ \dir -> do
  writeFile (dir ++ "/source") submission
  (code, output) <- runLinterIn dir ["source"]
  commands <- either fail pure (decodeCommands (T.pack output))
  pure (code, commands)

runLinter :: [String] -> IO (ExitCode, String)
runLinter args = withTempDirectory (`runLinterIn` args)

-- | Runs the compiled linter in a directory of its own: hlint reads .hlint.yaml from the working directory.
runLinterIn :: FilePath -> [String] -> IO (ExitCode, String)
runLinterIn dir args = do
  linter <- getEnv "JSON_LINTER"
  (code, output, _) <- readCreateProcessWithExitCode ((proc linter args) {cwd = Just dir}) ""
  pure (code, output)

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory action = do
  tmp <- getTemporaryDirectory
  (dir, handle) <- openTempFile tmp "lint"
  hClose handle
  removeFile dir
  createDirectory dir
  action dir `finally` removeDirectoryRecursive dir
