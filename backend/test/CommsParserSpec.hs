{-# LANGUAGE OverloadedStrings #-}

module CommsParserSpec (main, spec) where

import ArbitraryInstances
import BrainData
import Test.Hspec
import Test.QuickCheck
import CommsParser

prop_parsesFrontendMsgToCannonical :: FrontendMsgTest -> Bool
prop_parsesFrontendMsgToCannonical (FrontendMsgTest msg curl titl) = case parseEventMsg msg of
  Just (EventMsg url' title') -> curl == url' && titl == title'
  Nothing -> False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "CommsParser" $
    it "should parse the message" $ property prop_parsesFrontendMsgToCannonical
