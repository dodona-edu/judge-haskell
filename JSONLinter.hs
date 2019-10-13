{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                (liftM)
import           Data.Aeson                   (encode, Value, (.=), object)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
import           Language.Haskell.Exts.SrcLoc
import qualified Language.Haskell.HLint3      as HLint
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)

writeJSON :: Value -> IO ()
writeJSON = T.putStr . T.decodeUtf8 . encode

(.==) :: T.Text -> T.Text -> (T.Text, Value)
(.==) = (.=)

ideaToJSON :: HLint.Idea -> Value
ideaToJSON idea = object [ "command" .== "annotate-code"
                         , "type"    .= ((toString $ HLint.ideaSeverity idea) :: String)
                         , "row"     .= max 0 (row1 - 2) -- module line + zero-based
                         , "rows"    .= max 0 (row2 - 2) -- module line + zero-based
                         , "column"  .= max 0 (column1 - 1) -- zero-based
                         , "columns" .= max 0 (column2 - 1) -- zero-based
                         , "text"    .= (text (HLint.ideaHint idea) (HLint.ideaTo idea)) ]
    where (SrcSpan filename row1 column1 row2 column2) = HLint.ideaSpan idea
          toString HLint.Suggestion = "info"
          toString HLint.Warning    = "warning"
          toString HLint.Error      = "error"
          toString HLint.Ignore     = "info"
          text hint Nothing   = hint
          text hint (Just to) = hint ++ ". Why not: " ++ to


lint :: String -> IO ()
lint filename = mapM_ writeJSON =<< map ideaToJSON <$> HLint.hlint [filename, "--quiet"]

main :: IO ()
main = do args <- getArgs
          case args of
            [filename] -> lint filename
            _          -> exitFailure

