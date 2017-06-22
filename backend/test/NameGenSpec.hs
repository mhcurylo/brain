{-# LANGUAGE OverloadedStrings #-}

module NameGenSpec (main, spec) where
import qualified Data.Text                      as T

import NameGen
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = hspec spec

prop_runName :: Property
prop_runName = monadicIO $ do
   name <- run runName
   assert ((length . T.words $ name) == 2)

spec :: Spec
spec = do
  describe "NameGen runName" $ do
    it "should generate name of two words" $ property $ prop_runName
