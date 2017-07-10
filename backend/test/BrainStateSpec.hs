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
prop_isNameInUse_true name = isNameInUse name state
  where
    state = State (S.singleton name) M.empty M.empty M.empty

prop_isNameInUse_false :: Name -> State -> Bool
prop_isNameInUse_false name state = not $ isNameInUse name state

prop_addsUserToState :: UserUUID -> Name -> State -> Bool
prop_addsUserToState uuid name state = M.member uuid (stateUsers newState) && isNameInUse name newState
  where
    newState = addUserToState uuid name state

prop_removesUserFromState :: UserUUID -> Name -> State -> Bool
prop_removesUserFromState uuid name state = not $ isNameInUse name newState
  where
    newState = removeUserFromState uuid name $ addUserToState uuid name state

spec :: Spec
spec = do
  describe "isNameInUse" $ do
    it "should return true if name is in use" $ property prop_isNameInUse_true
    it "should return false if name is not in use" $ property prop_isNameInUse_false
  describe "addUserToState" $ do
    it "should add user to state" $ property prop_addsUserToState
  describe "removeUserFromState" $ do
    it "should remove added user from state" $ property prop_removesUserFromState