{-# LANGUAGE OverloadedStrings #-}

module BrainStateSpec (main, spec) where

import BrainState
import BrainData
import Test.Hspec
import Test.QuickCheck
import qualified Data.Text           as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as M
import qualified Data.Set            as S

main :: IO ()
main = hspec spec

prop_isNameInUse_true :: Name -> Bool
prop_isNameInUse_true name = isNameInUse name state || T.null name
  where
    state = State (S.singleton name) M.empty M.empty M.empty M.empty

prop_isNameInUse_false :: Name -> Bool
prop_isNameInUse_false name = not $ isNameInUse name initState

spec :: Spec
spec = do
  describe "isNameInUse" $ do
    it "should return true if name is in use" $ property prop_isNameInUse_true
    it "should return false if name is not in use" $ property prop_isNameInUse_false
