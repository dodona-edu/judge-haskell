
import           Control.Monad                (liftM)
import qualified Data.ByteString              as BL
import qualified Data.Json.Builder            as JSON
import           Data.Monoid                  ((<>))
import           Language.Haskell.Exts.SrcLoc
import qualified Language.Haskell.HLint3      as HLint
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)

writeJSON :: JSON.Value a => a -> IO ()
writeJSON = BL.putStr . JSON.toJsonBS

ideaToJSON :: HLint.Idea -> JSON.Object
ideaToJSON idea = JSON.row "command" "annotate-code"
               <> JSON.row "row" row
               <> JSON.row "column" column
               <> JSON.row "type" (toString $ HLint.ideaSeverity idea)
               <> JSON.row "rows" rows
               <> JSON.row "columns" columns
               <> JSON.row "text" (text (HLint.ideaHint idea) (HLint.ideaTo idea))
    where (SrcSpan filename row column rows columns) = HLint.ideaSpan idea
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

