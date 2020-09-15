{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                (liftM)
import           Data.Aeson                   (encode, Value, (.=), object)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
import           SrcLoc                       (SrcLoc(RealSrcLoc), srcLocLine, srcLocCol, srcSpanEnd, srcSpanStart)
import           Language.Haskell.HLint       (hlint, Idea, ideaSeverity, ideaSpan, ideaHint, ideaTo, Severity(..))
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)

writeJSON :: Value -> IO ()
writeJSON = T.putStr . T.decodeUtf8 . encode

(.==) :: T.Text -> T.Text -> (T.Text, Value)
(.==) = (.=)

ideaToJSON :: Idea -> Value
ideaToJSON idea = object [ "command" .== "annotate-code"
                         , "type"    .= ((toString $ ideaSeverity idea) :: String)
                         , "row"     .= (realLine $ srcSpanStart $ ideaSpan idea)
                         , "rows"    .= (realLine $ srcSpanEnd   $ ideaSpan idea)
                         , "column"  .= (realCol  $ srcSpanStart $ ideaSpan idea)
                         , "columns" .= (realCol  $ srcSpanEnd   $ ideaSpan idea)
                         , "text"    .= (text (ideaHint idea) (ideaTo idea)) ]
    where toString Suggestion     = "info"
          toString Warning        = "warning"
          toString Error          = "error"
          toString Ignore         = "info"
          text hint Nothing       = hint
          text hint (Just to)     = hint ++ ". Why not: " ++ to
          realLine (RealSrcLoc l) = max 0 (srcLocLine l - 2) -- module line + zero-based
          realLine _              = 0
          realCol  (RealSrcLoc l) = max 0 (srcLocCol l - 1) -- zero-based
          realCol  _              = 0

lint :: String -> IO ()
lint filename = mapM_ writeJSON =<< map ideaToJSON <$> hlint [filename, "--quiet"]

main :: IO ()
main = do args <- getArgs
          case args of
            [filename] -> lint filename
            _          -> exitFailure

