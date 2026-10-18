import           Test.Hspec

import qualified ConfigSpec
import qualified FeedbackSpec
import qualified HUnitJudgeSpec
import qualified JudgeMessagesSpec
import qualified LintSpec
import qualified QuickCheckJudgeSpec
import qualified UtilsSpec

main :: IO ()
main = hspec $ do
  describe "HUnitJudge" HUnitJudgeSpec.spec
  describe "QuickCheckJudge" QuickCheckJudgeSpec.spec
  describe "Utils" UtilsSpec.spec
  describe "Lint" LintSpec.spec
  describe "Config" ConfigSpec.spec
  describe "Feedback" FeedbackSpec.spec
  describe "JudgeMessages" JudgeMessagesSpec.spec
