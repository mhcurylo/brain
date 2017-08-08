{-# LANGUAGE OverloadedStrings #-}

module BrainStateSpec (main, spec) where

import ArbitraryInstances()
import BrainState
import BrainData
import BrainMsg  hiding (at)
import Test.Hspec
import Test.QuickCheck
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Time.Clock     as TC
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

prop_addsPlaceToState :: Name -> FrontendMsg -> UUid User -> TC.UTCTime -> State -> Bool
prop_addsPlaceToState name msg userUUid time state = isJust $ newState^.(statePlaces . at placeUUid)
  where
    (newState, _) = addEventToState msg userUUid time . addUserToState userUUid name $ state
    placeUUid = getUUid $ msg^.url

prop_addsPlacEventToState :: Name ->FrontendMsg -> UUid User -> TC.UTCTime -> State -> Bool
prop_addsPlacEventToState name msg userUUid time state = isJust (newState^.(statePlaceEvents . at uuid))
                                         &&  isJust (newState^?(stateUsers . at userUUid)._Just.userHistory.ix 0)
                                        && isJust (newState^?(statePlaces . at placeUUid)._Just.placeHistory.ix 0)
  where
    (newState, _) = addEventToState msg userUUid time . addUserToState userUUid name $ state
    url' = msg^.url
    placeUUid = getUUid url'
    uuid = getUUid $ PlaceEvent time userUUid placeUUid Nothing

prop_addsUserToMostRecentEvent :: Name -> FrontendMsg -> FrontendMsg -> UUid User -> TC.UTCTime -> State -> Bool
prop_addsUserToMostRecentEvent name msg msg' userUUid time state = userAtPlace placeUUid' && not (userAtPlace placeUUid)
  where
    (newState, _) = addEventToState msg' userUUid time . fst . addEventToState msg userUUid time . addUserToState userUUid name $ state
    placeUUid = getUUid $ msg^.url
    placeUUid' = getUUid $ msg'^.url
    userAtPlace uuid = fromMaybe False $ newState^?(statePlaces . at uuid)._Just.placeUsers.contains userUUid

prop_returnsProperUserLists :: Name -> Name -> FrontendMsg -> FrontendMsg -> UUid User -> UUid User -> TC.UTCTime -> State -> Bool
prop_returnsProperUserLists name name' msg msg' userUUid userUUid' time state = users == S.fromList [userUUid', userUUid]
  where
    (_, (users, _):_) = addEventToState msg userUUid' time . fst . addEventToState msg userUUid time  . fst . addEventToState msg' userUUid time . addUserToState userUUid name . addUserToState userUUid' name' $ state

prop_returnsProperFrontendReplies :: Name ->  FrontendMsg -> FrontendMsg -> UUid User -> TC.UTCTime -> State -> Bool
prop_returnsProperFrontendReplies name msg msg' userUUid time state = replyAt == replyAt' && replyFrom == replyFrom'
  where
    (_, (_, replyFrom):(_, replyAt):_) = addEventToState msg' userUUid time . fst . addEventToState msg userUUid time . addUserToState userUUid name $ state
    replyAt' = replyPageEvent $ PageEventPayload msg' (Just msg) msg' time name
    replyFrom' = replyPageEvent $ PageEventPayload msg' (Just msg) msg time name

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
    it "should return users-at-place"  $ property prop_returnsProperUserLists
    it "should return the frontendReply"  $ property prop_returnsProperFrontendReplies
