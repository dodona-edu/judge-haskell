{-# LANGUAGE OverloadedStrings #-}

-- Runs a submission: compiles the judge, the submission and each test file, and runs the tests.
-- Started by run with the cabal store's package db as argument and Dodona's config on stdin.

import           Control.Exception     (finally)
import           Control.Monad         (forM_, unless, when)
import qualified Data.ByteString       as BS
import           Data.List             (isPrefixOf, isSuffixOf, sort)
import           Data.Text             (Text)
import           System.Directory
import           System.Environment    (getArgs, setEnv)
import           System.Exit
import           System.FilePath       (takeFileName, (</>))
import           System.IO             (hClose, openBinaryTempFile)
import           System.Process

import           Config
import           Feedback
import           JudgeMessages         (JudgeMessage (..), languageCode, languageVariable, translate)
import           Lint

main :: IO ()
main = do
  args <- getArgs
  pkgdb <- case args of
    [pkgdb] -> pure pkgdb
    _       -> die "usage: Judge <package db>"
  config <- either die pure . parseConfig =<< BS.getContents
  builddir <- takeWhile (/= '\n') <$> readProcess "mktemp" ["-d", "-p", "/dev/shm"] ""
  judgeSubmission config pkgdb builddir `finally` removeDirectoryRecursive builddir

judgeSubmission :: Config -> FilePath -> FilePath -> IO ()
judgeSubmission config pkgdb builddir = do
  let text = translate (naturalLanguage config)
  setEnv languageVariable (languageCode (naturalLanguage config))

  -- Let's get started
  write StartJudgement

  -- Compiling the judge code
  setCurrentDirectory (judge config)
  judgeModules <- libModules
  compileOr
    (\output ->
      [ AppendMessage (Message Code output (Just Staff))
      , CloseJudgement (Just False) (Just (Status InternalError (text JudgeCompilationFailed))) ])
    (["-outputdir", builddir, "-package-db", pkgdb] ++ judgeModules)

  -- Create the Input.hs module, containing the submitted code
  BS.writeFile "lib/Input.hs" . ("module Input where\n" <>) =<< BS.readFile (source config)

  -- Lint the user code.
  mapM_ write =<< lint

  -- Compiling the user code.
  write (StartTab (text CompilationTab) True Nothing)
  write (StartContext (Just (Message Code "ghc Input.hs" Nothing)))
  submissionModules <- libModules
  compileOr
    (\output ->
      [ AppendMessage (Message Code output Nothing)
      , CloseContext (Just False)
      , CloseTab
      , CloseJudgement (Just False) (Just (Status CompilationError (text CompilationFailed))) ])
    (["-outputdir", builddir, "-package-db", pkgdb] ++ submissionModules)
  write (CloseContext Nothing)
  write CloseTab

  -- Copy over non-testfiles
  resourceFiles <- filesUnder (resources config)
  forM_ resourceFiles $ \file ->
    when (isResourceModule (takeFileName file)) $ copyFile file ("lib" </> takeFileName file)

  let typecheck = resources config </> "typecheck.hs"
  hasTypecheck <- doesFileExist typecheck
  when hasTypecheck $ do
    write (StartTab "Typecheck" True Nothing)
    BS.writeFile "lib/Typecheck.hs" . ("module Typecheck where\n" <>) =<< BS.readFile typecheck
    typecheckModules <- libModules
    compileOr
      (\output ->
        [ StartContext (Just (Message Plain (text CompilationContext) Nothing))
        , AppendMessage (Message Code output Nothing)
        , CloseContext (Just False)
        , CloseTab
        , CloseJudgement (Just False) (Just (Status CompilationError (text InterfaceNotSatisfied))) ])
      (["-i" ++ builddir, "-outputdir", builddir, "-package-db", pkgdb] ++ typecheckModules)
    write CloseTab

  -- Compiling and running each test file.
  tests <- testFiles (resources config)
  forM_ tests $ \testFile -> do
    exists <- doesFileExist testFile
    when exists $ do
      -- Compiling the test
      write (StartTab (tabTitle testFile) True (Just Staff))
      copyFile testFile "src/Test.hs"
      compileOr
        (\output ->
          [ StartContext (Just (Message Plain (text CompilationContext) Nothing))
          , AppendMessage (Message Code output Nothing)
          , CloseContext (Just False)
          , CloseTab
          , CloseJudgement (Just False) (Just (Status InternalError (text TestCompilationFailed))) ])
        ["-i" ++ builddir, "-outputdir", builddir, "-package-db", pkgdb, "-c", "src/Test.hs"]
      write CloseTab

      -- Running the test
      write (StartTab (tabTitle testFile) False Nothing)
      code <- withCreateProcess
        (proc "runghc" [ "-w", "-package-db", "--ghc-arg=" ++ pkgdb
                       , "-i" ++ builddir ++ ":" ++ judge config ++ "/lib", judge config ++ "/src/Test.hs" ])
          { cwd = Just (workdir config) }
        (\_ _ _ process -> waitForProcess process)
      -- A failing test program ends the judgement as is: Dodona closes what is still open.
      unless (code == ExitSuccess) $ exitWith (shellExitCode code)
      write CloseTab

  write (CloseJudgement Nothing Nothing)

-- | The annotations for lib/Input.hs from hlint, which exits non-zero when it finds hints.
-- Run by path because the runner's PATH lists root's directories only.
lint :: IO [Command]
lint = do
  home <- getHomeDirectory
  (_, Just out, _, process) <- createProcess
    (proc (home </> ".local/bin/hlint") ["--json", "--quiet", "lib/Input.hs"]) { std_out = CreatePipe }
  output <- BS.hGetContents out
  _ <- waitForProcess process
  pure (either (const []) (map annotation) (parseIdeas output))

-- | Runs ghc; when it fails, writes the commands for its output and ends the judgement.
compileOr :: (Text -> [Command]) -> [String] -> IO ()
compileOr onFailure args = do
  (code, output) <- ghc args
  unless (code == ExitSuccess) $ do
    mapM_ write (onFailure output)
    exitSuccess

-- | Runs ghc with its stdout and stderr interleaved in one file.
ghc :: [String] -> IO (ExitCode, Text)
ghc args = do
  tmp <- getTemporaryDirectory
  (path, handle) <- openBinaryTempFile tmp "compilation"
  code <- withCreateProcess (proc "ghc" args) { std_out = UseHandle handle, std_err = UseHandle handle }
    (\_ _ _ process -> waitForProcess process)
  hClose handle
  output <- BS.readFile path
  removeFile path
  pure (code, compilerOutput output)

-- | lib/*.hs, in the order of the shell's glob.
libModules :: IO [FilePath]
libModules = map ("lib" </>) . sort . filter isModule <$> listDirectory "lib"
  where isModule name = ".hs" `isSuffixOf` name && not ("." `isPrefixOf` name)

-- | The files under a directory, recursively, in the order find lists them.
filesUnder :: FilePath -> IO [FilePath]
filesUnder dir = concat <$> (mapM visit =<< listDirectory dir)
  where
    visit name = do
      let path = dir </> name
      isLink <- pathIsSymbolicLink path
      isDirectory <- doesDirectoryExist path
      if isDirectory && not isLink then filesUnder path
      else pure [path | not isDirectory]

-- | The exit code sh reports for a process: 128 + n when signal n killed it.
shellExitCode :: ExitCode -> ExitCode
shellExitCode (ExitFailure n) | n < 0 = ExitFailure (128 - n)
shellExitCode code                    = code
