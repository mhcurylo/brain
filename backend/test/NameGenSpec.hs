{-# LANGUAGE OverloadedStrings #-}

module NameGenSpec (main, spec) where
import qualified Data.Text   as T

import ArbitraryInstances()
import BrainData
import NameGen
import Control.Lens
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = hspec spec

prop_runName_returns2words :: Property
prop_runName_returns2words = monadicIO $ do
   name <- run runName
   assert $ (length . T.words $ name^.nAME) >= 2

spec :: Spec
spec = do
  describe "NameGen runName" $ do
    it "should generate name of two words" $ property prop_runName_returns2words
