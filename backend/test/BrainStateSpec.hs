{-# LANGUAGE OverloadedStrings #-}

module BrainStateSpec (main, spec) where

import ArbitraryInstances()
import BrainState
import BrainData
import Test.Hspec
import Test.QuickCheck
import qualified Data.Map            as M
import qualified Data.Set            as S
import Control.Lens
import Control.Lens.At
import Data.Maybe

main :: IO ()
main = hspec spec

prop_isNameInUse_true :: Name -> Bool
prop_isNameInUse_true name = isNameInUse name state
  where
    state = State (S.singleton name) M.empty M.empty M.empty

prop_isNameInUse_false :: Name -> State -> Bool
prop_isNameInUse_false name state = not $ isNameInUse name state

prop_addsUserToState :: UserUUID -> Name -> State -> Bool
prop_addsUserToState uuid name state = containsUser && isNameInUse name newState
  where
    newState = addUserToState uuid name state
    containsUser = isJust . (^. stateUsers . at uuid) $ newState

prop_removesUserFromState :: UserUUID -> Name -> State -> Bool
prop_removesUserFromState uuid name state = not $ isNameInUse name newState
  where
    newState = removeUserFromState uuid name $ addUserToState uuid name state

prop_addsPlaceToState :: Name -> EventData -> State -> Bool
prop_addsPlaceToState name event state = isJust $ newState^.(statePlaces . at uuid)
  where
    newState = fst . addEventToState event . addUserToState (event^.eventDataUserUUID) name $ state
    uuid = urlUUID $ event^.(eventDataEventMsg.eventMsgUrl)

prop_addsPlaceEventToState :: Name -> EventData -> State -> Bool
prop_addsPlaceEventToState name event state = isJust $ newState^.(statePlaces . at uuid)
  where
    newState = fst . addEventToState event . addUserToState (event^.eventDataUserUUID) name $ state
    uuid = urlUUID $ event^.(eventDataEventMsg.eventMsgUrl)

spec :: Spec
spec = do
  describe "isNameInUse" $ do
    it "should return true if name is in use" $ property prop_isNameInUse_true
    it "should return false if name is not in use" $ property prop_isNameInUse_false
  describe "addUserToState" $
    it "should add user to state" $ property prop_addsUserToState
  describe "removeUserFromState" $
    it "should remove added user from state" $ property prop_removesUserFromState
  describe "addEventToState" $
    it "should create a place if place is not there" $ property prop_addsPlaceToState
