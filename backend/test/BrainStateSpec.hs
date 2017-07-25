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
import Data.Maybe

main :: IO ()
main = hspec spec

prop_isNameInUse_true :: Name -> Bool
prop_isNameInUse_true name = isNameInUse name state
  where
    state = State (S.singleton name) M.empty M.empty M.empty

prop_isNameInUse_false :: Name -> State -> Bool
prop_isNameInUse_false name state = not $ isNameInUse name state

prop_addsUserToState :: UUid User -> Name -> State -> Bool
prop_addsUserToState uuid name state = containsUser && isNameInUse name newState
  where
    newState = addUserToState uuid name state
    containsUser = isJust . (^. stateUsers . at uuid) $ newState

prop_removesUserFromState :: UUid User -> Name -> State -> Bool
prop_removesUserFromState uuid name state = not $ isNameInUse name newState
  where
    newState = removeUserFromState uuid name $ addUserToState uuid name state

prop_addsPlaceToState :: Name ->  EventData -> State -> Bool
prop_addsPlaceToState name event state = isJust $ newState^.(statePlaces . at uuid)
  where
    (newState, _) = addEventToState event . addUserToState (event^.eventDataUserUUid) name $ state
    uuid = getUUid $ event^.(eventDataEventMsg.eventMsgUrl)

prop_addsPlacEventToState :: Name -> EventData -> State -> Bool
prop_addsPlacEventToState name event state = isJust (newState^.(statePlaceEvents . at uuid))
                                         &&  isJust (newState^?(stateUsers . at userUUid)._Just.userHistory.ix 0)
                                        && isJust (newState^?(statePlaces . at placeUUid)._Just.placeHistory.ix 0)
  where
    (newState, _) = addEventToState event . addUserToState (event^.eventDataUserUUid) name $ state
    time = event^.eventDataTime
    userUUid = event^.eventDataUserUUid
    url' = event^.eventDataEventMsg^.eventMsgUrl
    placeUUid = getUUid url'
    uuid = getUUid $ PlaceEvent time userUUid placeUUid Nothing

prop_addsUserToMostRecentEvent :: Name -> EventData -> EventData -> State -> Bool
prop_addsUserToMostRecentEvent name event event' state = userAtPlace placeUUid' && not (userAtPlace placeUUid)
  where
    (newState, _) = addEventToState (event' & eventDataUserUUid .~ userUUid) . fst . addEventToState event . addUserToState userUUid name $ state
    userUUid = event^.eventDataUserUUid
    placeUUid = getUUid $ event^.eventDataEventMsg^.eventMsgUrl
    placeUUid' = getUUid $ event'^.eventDataEventMsg^.eventMsgUrl
    userAtPlace uuid = fromMaybe False $ newState^?(statePlaces . at uuid)._Just.placeUsers.contains userUUid

spec = do
  describe "isNameInUse" $ do
    it "should return true if name is in use" $ property prop_isNameInUse_true
    it "should return false if name is not in use" $ property prop_isNameInUse_false
  describe "addUserToState" $
    it "should add user to state" $ property prop_addsUserToState
  describe "removeUserFromState" $
    it "should remove added user from state" $ property prop_removesUserFromState
  describe "addEventToState" $ do
    it "should create a place if place is not there" $ property prop_addsPlaceToState
    it "should add PlaceEvent to state" $ property prop_addsPlacEventToState
    it "should clean the user from old place"  $ property prop_addsUserToMostRecentEvent
