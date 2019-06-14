
module QuickCheckJudge where

import qualified Data.ByteString           as BL
import qualified Data.Json.Builder         as JSON
import           Data.Monoid               ((<>))
import           Test.QuickCheck.Arbitrary (Arbitrary)
import           Test.QuickCheck.Test      (Args (..), Result, isSuccess,
                                            output, quickCheckWithResult)

-- A testcase consists of a description together with a property to be tested
data TestCase a = TestCase
  { description :: String    -- Description of the Test case
  , prop        :: a -> Bool -- Property to Quickcheck
  }

-- The result of a test: whether it's accepted and why (not).
data CheckedResult = CheckedResult
  { accepted :: Bool
  , message  :: String
  }

-- We have custom arguments in order to make sure that
-- quickcheck is silent, currently we reuse the default
-- parameters except for chatty which we put on False
judgeArgs :: Args
judgeArgs = Args
  { replay          = Nothing
  , maxSuccess      = 100
  , maxDiscardRatio = 10
  , maxSize         = 100
  , chatty          = False
  , maxShrinks      = 100
  }

writeJSON :: JSON.Value a => a -> IO ()
writeJSON = BL.putStr . JSON.toJsonBS

checkResult :: Result -> CheckedResult
checkResult result = CheckedResult (isSuccess result) (output result)

executeTestCase :: (Arbitrary a, Show a) => TestCase a -> IO ()
executeTestCase (TestCase d p) = writeTestCase =<< checkResult <$> quickCheckWithResult judgeArgs p
  where writeTestCase result =
            mapM_ writeJSON [ JSON.row "command" "start-context"
                            , JSON.row "command" "start-testcase" <>
                              JSON.row "description" (JSON.row "format" "haskell" <>
                                                      JSON.row "description" d)
                            , JSON.row "command" "append-message" <>
                              JSON.row "message" (JSON.row "format" "code" <>
                                                  JSON.row "description" (message result))
                            , JSON.row "command" "close-testcase" <>
                              JSON.row "accepted" (accepted result)
                            , JSON.row "command" "escalate-status" <>
                              JSON.row "status" (if accepted result
                                then JSON.row "human" "Correct" <> JSON.row "enum" "correct"
                                else JSON.row "human" "Wrong" <> JSON.row "enum" "wrong")
                            , JSON.row "command" "close-context"
                            ]

