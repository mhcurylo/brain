{-# LANGUAGE OverloadedStrings #-}

module CommsParserSpec (main, spec) where

import ArbitraryInstances
import BrainData
import Test.Hspec
import Test.QuickCheck
import CommsParser
import qualified Data.ByteString         as B

prop_parsesFrontendMsg :: ArbitraryFrontendMsg -> Bool
prop_parsesFrontendMsg (ArbitraryFrontendMsg msg curl title) = case parseEventMsg msg of
  Just (EventMsg url' title') -> curl == url' && title == title'
  Nothing -> false

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "CommsParser" $
    it "should parse the message" $ property prop_runName_returns2words
