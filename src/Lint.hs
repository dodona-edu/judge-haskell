{-# LANGUAGE OverloadedStrings #-}

module Lint
  ( Idea (..)
  , parseIdeas
  , annotation
  ) where

import           Data.Aeson           (FromJSON (..), eitherDecodeStrict, withObject, (.:), (.:?))
import qualified Data.ByteString      as BS
import           Data.Text            (Text)

import           Feedback             (Command (..))

-- | The fields of an idea in the output of hlint --json that the annotation uses.
data Idea = Idea
  { severity    :: Text
  , hint        :: Text
  , replacement :: Maybe Text
  , startLine   :: Int
  , startColumn :: Int
  , endLine     :: Int
  , endColumn   :: Int
  } deriving (Eq, Show)

instance FromJSON Idea where
  parseJSON = withObject "idea" $ \o ->
    Idea <$> o .: "severity" <*> o .: "hint" <*> o .:? "to"
         <*> o .: "startLine" <*> o .: "startColumn" <*> o .: "endLine" <*> o .: "endColumn"

parseIdeas :: BS.ByteString -> Either String [Idea]
parseIdeas = eitherDecodeStrict

-- | The annotation for an idea in lib/Input.hs, which has a module line above the submission.
-- Positions are zero-based. rows and columns are the end position, not a count (#26).
annotation :: Idea -> Command
annotation idea =
  AnnotateCode (kind (severity idea))
    (zeroBased 2 (startLine idea)) (zeroBased 1 (startColumn idea))
    (zeroBased 2 (endLine idea)) (zeroBased 1 (endColumn idea))
    (maybe (hint idea) (\to -> hint idea <> ". Why not: " <> to) (replacement idea))
  where
    zeroBased offset position = max 0 (position - offset)
    kind "Error"   = "error"
    kind "Warning" = "warning"
    kind _         = "info"
