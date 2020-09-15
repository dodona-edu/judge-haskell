{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                (liftM)
import           Data.Aeson                   (encode, Value, (.=), object)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
import qualified Language.Haskell.HLint as H
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)

writeJSON :: Value -> IO ()
writeJSON = T.putStr . T.decodeUtf8 . encode

(.==) :: T.Text -> T.Text -> (T.Text, Value)
(.==) = (.=)

ideaToJSON :: H.Idea -> Value
ideaToJSON idea = object [ "command" .== "annotate-code"
                         , "type"    .= ((toString $ H.ideaSeverity idea) :: String)
                         , "row"     .= max 0 (srcSpan fst fst idea - 2) -- module line + zero-based
                         , "rows"    .= max 0 (srcSpan snd fst idea - 2) -- module line + zero-based
                         , "column"  .= max 0 (srcSpan fst snd idea - 1) -- zero-based
                         , "columns" .= max 0 (srcSpan snd snd idea - 1) -- zero-based
                         , "text"    .= (text (H.ideaHint idea) (H.ideaTo idea)) ]
    where toString H.Suggestion = "info"
          toString H.Warning    = "warning"
          toString H.Error      = "error"
          toString H.Ignore     = "info"
          text hint Nothing     = hint
          text hint (Just to)   = hint ++ ". Why not: " ++ to
          srcSpan se lc         = let select (_, s, e) = lc $ se (s, e)
                                   in maybe 0 select . H.unpackSrcSpan . H.ideaSpan

lint :: String -> IO ()
lint filename = mapM_ writeJSON =<< map ideaToJSON <$> H.hlint [filename, "--quiet"]

main :: IO ()
main = do args <- getArgs
          case args of
            [filename] -> lint filename
            _          -> exitFailure

