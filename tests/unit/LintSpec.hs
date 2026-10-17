{-# LANGUAGE OverloadedStrings #-}

module LintSpec (spec) where

import           Data.Aeson      (Value (..), toJSON, (.=))
import qualified Data.ByteString as BS
import           Data.Either     (isLeft)
import           Test.Hspec

import           Feedback
import           Lint
import           Support

spec :: Spec
spec = do
  describe "annotation" $ do
    it "writes nothing for code without hints" $
      annotations "[]" `shouldBe` []

    it "annotates a hint at its zero-based row in the submission and zero-based column" $ do
      let written = map toJSON (annotations redundantBracket)
      fieldsOf "annotate-code" ["row"] written `shouldBe` [Number 0]
      fieldsOf "annotate-code" ["column"] written `shouldBe` [Number 6]

    it "writes one annotate-code command per hint" $
      annotations redundantBracket `shouldBe`
        [AnnotateCode "warning" 0 6 0 9 "Redundant bracket. Why not: x"]

    it "writes the command with its type, positions and text" $
      map toJSON (annotations redundantBracket) `shouldBe`
        [ command "annotate-code"
            [ "type" .= String "warning", "row" .= Number 0, "column" .= Number 6
            , "rows" .= Number 0, "columns" .= Number 9, "text" .= String "Redundant bracket. Why not: x" ] ]

    it "sends the zero-based end row and end column as rows and columns" $
      annotations multiLine `shouldBe`
        [AnnotateCode "warning" 0 7 2 12 "Use concatMap. Why not: concatMap show xs"]

    it "annotates every hint, in the order hlint gives them" $
      annotations twoHints `shouldBe`
        [ AnnotateCode "warning" 0 7 0 27 "Use concatMap. Why not: concatMap show xs"
        , AnnotateCode "warning" 1 6 1 15 "Redundant ==. Why not: x"
        ]

    it "maps hlint's suggestions to info" $
      annotations suggestion `shouldBe` [AnnotateCode "info" 0 0 0 12 "Use camelCase. Why not: myValue = ..."]

    it "maps hlint's ignored hints to info" $
      annotations (single "Ignore" "\"to\":\"x\"") `shouldBe`
        [AnnotateCode "info" 0 6 0 9 "Redundant bracket. Why not: x"]

    it "maps hlint's warnings to warning" $
      fieldsOf "annotate-code" ["type"] (map toJSON (annotations redundantBracket)) `shouldBe` ["warning"]

    it "maps a parse error to error, without a suggested replacement" $
      annotations parseError `shouldBe` [AnnotateCode "error" 0 6 0 7 "Parse error: on input `='"]

    it "keeps a replacement that is empty" $
      annotations (single "Warning" "\"to\":\"\"") `shouldBe`
        [AnnotateCode "warning" 0 6 0 9 "Redundant bracket. Why not: "]

    it "marks the number of rows a hint spans" $
      knownBug "#26 rows/columns are end positions" $
        fieldsOf "annotate-code" ["rows"] (map toJSON (annotations multiLine)) `shouldBe` [Number 3]

    it "marks one row and the width of a single-line hint" $
      knownBug "#26 rows/columns are end positions" $
        map ((\a -> (field ["rows"] a, field ["columns"] a)) . toJSON) (annotations redundantBracket)
          `shouldBe` [(Just (Number 1), Just (Number 3))]

  describe "parseIdeas" $ do
    it "reads the fields of each idea and ignores the others" $
      parseIdeas twoHints `shouldBe` Right
        [ Idea "Warning" "Use concatMap" (Just "concatMap show xs") 2 8 2 28
        , Idea "Warning" "Redundant ==" (Just "x") 3 7 3 16
        ]

    it "reads a null replacement as none" $
      fmap (map replacement) (parseIdeas parseError) `shouldBe` Right [Nothing]

    it "fails on output that is no list of ideas" $
      parseIdeas "hlint: not found" `shouldSatisfy` isLeft

annotations :: BS.ByteString -> [Command]
annotations = either error (map annotation) . parseIdeas

-- Output of the hlint executable, `hlint --json --quiet lib/Input.hs`, with the refactorings emptied.

-- f x = (x)
redundantBracket :: BS.ByteString
redundantBracket = single "Warning" "\"to\":\"x\""

-- | The redundant bracket idea with the given severity and "to" field.
single :: BS.ByteString -> BS.ByteString -> BS.ByteString
single severity to = BS.concat
  [ "[{\"module\":[\"Input\"],\"decl\":[\"f\"],\"severity\":\"", severity, "\",\"hint\":\"Redundant bracket\""
  , ",\"file\":\"lib/Input.hs\",\"startLine\":2,\"startColumn\":7,\"endLine\":2,\"endColumn\":10"
  , ",\"from\":\"(x)\",", to, ",\"note\":[],\"refactorings\":\"[]\"}]" ]

-- h xs = concat\n  (map\n    show xs)
multiLine :: BS.ByteString
multiLine =
  "[{\"module\":[\"Input\"],\"decl\":[\"h\"],\"severity\":\"Warning\",\"hint\":\"Use concatMap\"\
  \,\"file\":\"lib/Input.hs\",\"startLine\":2,\"startColumn\":8,\"endLine\":4,\"endColumn\":13\
  \,\"from\":\"concat (map show xs)\",\"to\":\"concatMap show xs\",\"note\":[],\"refactorings\":\"[]\"}]"

-- g xs = concat (map show xs)\nn x = x == True
twoHints :: BS.ByteString
twoHints =
  "[{\"module\":[\"Input\"],\"decl\":[\"g\"],\"severity\":\"Warning\",\"hint\":\"Use concatMap\"\
  \,\"file\":\"lib/Input.hs\",\"startLine\":2,\"startColumn\":8,\"endLine\":2,\"endColumn\":28\
  \,\"from\":\"concat (map show xs)\",\"to\":\"concatMap show xs\",\"note\":[],\"refactorings\":\"[]\"}\n\
  \,{\"module\":[\"Input\"],\"decl\":[\"n\"],\"severity\":\"Warning\",\"hint\":\"Redundant ==\"\
  \,\"file\":\"lib/Input.hs\",\"startLine\":3,\"startColumn\":7,\"endLine\":3,\"endColumn\":16\
  \,\"from\":\"x == True\",\"to\":\"x\",\"note\":[],\"refactorings\":\"[]\"}]"

-- my_value = 3
suggestion :: BS.ByteString
suggestion =
  "[{\"module\":[\"Input\"],\"decl\":[\"my_value\"],\"severity\":\"Suggestion\",\"hint\":\"Use camelCase\"\
  \,\"file\":\"lib/Input.hs\",\"startLine\":2,\"startColumn\":1,\"endLine\":2,\"endColumn\":13\
  \,\"from\":\"my_value = ...\",\"to\":\"myValue = ...\",\"note\":[],\"refactorings\":\"[]\"}]"

-- f x = = 3
parseError :: BS.ByteString
parseError =
  "[{\"module\":[],\"decl\":[],\"severity\":\"Error\",\"hint\":\"Parse error: on input `='\"\
  \,\"file\":\"lib/Input.hs\",\"startLine\":2,\"startColumn\":7,\"endLine\":2,\"endColumn\":8\
  \,\"from\":\"  module Input where\\n> f x = = 3\\n\",\"to\":null,\"note\":[],\"refactorings\":\"[]\"}]"
