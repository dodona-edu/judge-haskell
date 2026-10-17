{-# LANGUAGE OverloadedStrings #-}

module Feedback
  ( Command (..)
  , Message (..)
  , Format (..)
  , Permission (..)
  , Status (..)
  , StatusEnum (..)
  , write
  , tabTitle
  , compilerOutput
  ) where

import           Data.Aeson               (ToJSON (..), Value (..), encode, object, (.=))
import           Data.Aeson.Key           (Key)
import           Data.Aeson.Types         (Pair)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8Lenient)
import           System.FilePath          (takeFileName)
import           System.IO                (hFlush, stdout)

-- | The partial output commands the driver writes; the test programs write the rest themselves.
data Command
  = StartJudgement
  | StartTab Text Bool (Maybe Permission) -- title, hidden
  | StartContext (Maybe Message)          -- description
  | AppendMessage Message
  | CloseContext (Maybe Bool)             -- accepted
  | CloseTab
  | CloseJudgement (Maybe Bool) (Maybe Status) -- accepted
  | AnnotateCode Text Int Int Int Int Text -- type, row, column, rows, columns, text
  deriving (Eq, Show)

data Message = Message Format Text (Maybe Permission) deriving (Eq, Show)

data Format = Plain | Code deriving (Eq, Show)

data Permission = Staff deriving (Eq, Show)

data Status = Status StatusEnum Text deriving (Eq, Show)

data StatusEnum = InternalError | CompilationError deriving (Eq, Show)

instance ToJSON Command where
  toJSON StartJudgement = command "start-judgement" []
  toJSON (StartTab title hidden permission) =
    command "start-tab" (("title" .= title) : ["hidden" .= True | hidden] ++ maybeField "permission" permission)
  toJSON (StartContext description) = command "start-context" (maybeField "description" description)
  toJSON (AppendMessage message) = command "append-message" ["message" .= message]
  toJSON (CloseContext accepted) = command "close-context" (maybeField "accepted" accepted)
  toJSON CloseTab = command "close-tab" []
  toJSON (CloseJudgement accepted status) =
    command "close-judgement" (maybeField "accepted" accepted ++ maybeField "status" status)

  toJSON (AnnotateCode kind row column rows columns text) =
    command "annotate-code"
      ["type" .= kind, "row" .= row, "column" .= column, "rows" .= rows, "columns" .= columns, "text" .= text]

instance ToJSON Message where
  toJSON (Message format description permission) =
    object (["format" .= format, "description" .= description] ++ maybeField "permission" permission)

instance ToJSON Format where
  toJSON Plain = String "plain"
  toJSON Code  = String "code"

instance ToJSON Permission where
  toJSON Staff = String "staff"

instance ToJSON Status where
  toJSON (Status enum human) = object ["enum" .= enum, "human" .= human]

instance ToJSON StatusEnum where
  toJSON InternalError    = String "internal error"
  toJSON CompilationError = String "compilation error"

command :: Text -> [Pair] -> Value
command name pairs = object (("command" .= name) : pairs)

maybeField :: ToJSON a => Key -> Maybe a -> [Pair]
maybeField key = maybe [] (\value -> [key .= value])

-- | Writes a command and flushes, so it comes before what the processes started next write.
write :: Command -> IO ()
write c = BL.putStr (encode c) >> hFlush stdout

-- | The test name with spaces for underscores: count_words_test.hs becomes "count words".
tabTitle :: FilePath -> Text
tabTitle testFile = T.replace "_" " " (fromMaybe base (T.stripSuffix "_test.hs" base))
  where base = T.pack (takeFileName testFile)

-- | GHC's output as message text, without its trailing newlines.
compilerOutput :: BS.ByteString -> Text
compilerOutput = T.dropWhileEnd (== '\n') . decodeUtf8Lenient
