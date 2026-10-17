{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                (liftM)
import           Data.Aeson                   (encode, Value, (.=), object)
import           Data.Aeson.Key               (Key, fromText)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
import qualified Language.Haskell.HLint as H
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)

writeJSON :: Value -> IO ()
writeJSON = T.putStr . T.decodeUtf8 . encode

(.==) :: T.Text -> T.Text -> (Key, Value)
(.==) k = (.=) (fromText k)

ideaToJSON :: H.Idea -> Value
ideaToJSON idea = object [ "command" .== "annotate-code"
                         , "type"    .= ((toString $ H.ideaSeverity idea) :: String)
                         , "row"     .= (startLine - 1) -- zero-based
                         , "rows"    .= (endLine - startLine + 1)
                         , "column"  .= (startColumn - 1) -- zero-based
                         , "columns" .= columns
                         , "text"    .= (text (H.ideaHint idea) (H.ideaTo idea)) ]
    where toString H.Suggestion = "info"
          toString H.Warning    = "warning"
          toString H.Error      = "error"
          toString H.Ignore     = "info"
          text hint Nothing     = hint
          text hint (Just to)   = hint ++ ". Why not: " ++ to
          ((startLine, startColumn), (endLine, endColumn)) =
              maybe ((1, 1), (1, 1)) (\(_, s, e) -> (s, e)) (H.unpackSrcSpan (H.ideaSpan idea))
          -- Dodona counts the last row's columns from column on a single row, from the row's start otherwise
          columns | startLine == endLine = endColumn - startColumn
                  | otherwise            = endColumn - 1

lint :: String -> IO ()
lint filename = mapM_ writeJSON =<< map ideaToJSON <$> H.hlint [filename, "--quiet"]

main :: IO ()
main = do args <- getArgs
          case args of
            [filename] -> lint filename
            _          -> exitFailure

