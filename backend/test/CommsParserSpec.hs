{-# LANGUAGE OverloadedStrings #-}

module CommsParserSpec (main, spec) where

import ArbitraryInstances
import BrainMsg
import Test.Hspec
import Test.QuickCheck
import CommsParser

prop_processNormalizesFrontendMsgToCannonical :: FrontendMsgTest -> Bool
prop_processNormalizesFrontendMsgToCannonical (FrontendMsgTest msg curl titl) = case processFrontendMsg msg of
  Just (FrontendMsg url' title', CanonicalUrlReply _ (CanonicalUrlPayload _ cann)) -> url' == curl && cann == curl && titl == title'
  Just _ -> False
  Nothing -> False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "CommsParser" $
    it "should parse the message" $ property prop_processNormalizesFrontendMsgToCannonical
