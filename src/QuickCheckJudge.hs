{-# LANGUAGE OverloadedStrings #-}

module QuickCheckJudge where

import           Data.Aeson                   (encode, Value, (.=), object)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
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

writeJSON :: Value -> IO ()
writeJSON = T.putStr . T.decodeUtf8 . encode

(.==) :: T.Text -> T.Text -> (T.Text, Value)
(.==) = (.=)

checkResult :: Result -> CheckedResult
checkResult result = CheckedResult (isSuccess result) (output result)

executeTestCase :: (Arbitrary a, Show a) => TestCase a -> IO ()
executeTestCase (TestCase d p) = writeTestCase =<< checkResult <$> quickCheckWithResult judgeArgs p
  where writeTestCase result =
            mapM_ (writeJSON . object)
                [ [ "command" .== "start-context" ]
                , [ "command" .== "start-testcase"
                  , "description" .= object [ "format" .== "haskell"
                                            , "description" .= d ] ]
                , [ "command" .== "append-message"
                  , "message" .= object [ "format" .== "code"
                                        , "description" .= message result ] ]
                , [ "command" .== "close-testcase"
                  , "accepted" .= accepted result ]
                , [ "command" .== "escalate-status"
                  , "status" .= object (if accepted result
                    then [ "human" .== "Correct", "enum" .== "correct" ]
                    else [ "human" .== "Wrong",   "enum" .== "wrong" ]) ]
                , [ "command" .== "close-context" ]
                ]

