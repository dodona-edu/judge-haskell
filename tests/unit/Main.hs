import           Test.Hspec

import qualified HUnitJudgeSpec
import qualified JSONLinterSpec
import qualified QuickCheckJudgeSpec
import qualified UtilsSpec

main :: IO ()
main = hspec $ do
  describe "HUnitJudge" HUnitJudgeSpec.spec
  describe "QuickCheckJudge" QuickCheckJudgeSpec.spec
  describe "Utils" UtilsSpec.spec
  describe "JSONLinter" JSONLinterSpec.spec
