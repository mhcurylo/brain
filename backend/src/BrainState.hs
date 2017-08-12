{-# LANGUAGE DataKinds #-}
module BrainState where

import BrainData
import BrainMsg hiding (at)
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Data.Time.Clock     as TC
import Control.Lens.At               as L
import Control.Lens
import Debug.Trace

initState :: State
initState = State S.empty M.empty M.empty M.empty

initComms :: Connections
initComms = M.empty

isNameInUse :: Name -> State -> Bool
isNameInUse  name = (^. stateNamesInUse . contains name)

freshUser :: UUid User -> Name -> State -> State
freshUser uuid name = stateUsers . at uuid ?~ User name [] uuid

removeUserFromState :: UUid User -> Name -> State -> State
removeUserFromState uuid name = (stateNamesInUse . contains name).~False

addUserToState :: UUid User -> Name -> State -> State
addUserToState uuid name = (stateNamesInUse . contains name .~ True) . freshUser uuid name

addEventToState :: FrontendMsg -> UUid User -> TC.UTCTime -> State -> (State, FrontendReplies)
addEventToState (FrontendMsg url' title') userUUid time state =
  maybe id findOtherInterestedPlaces previousPlace
  . readyReply placeEvent
  . propagatePlaceEvent placeEvent
  . ensurePlaceExists url' title'$ state
  where
      placeUUid = getUUid url'
      previousPlace = lastVisited userUUid state
      placeEvent = PlaceEvent time userUUid placeUUid previousPlace

lastVisited :: UUid User -> State -> Maybe (UUid URL)
lastVisited userUUid state = lastPlace' <$> state^?stateUsers.at userUUid._Just.userHistory.ix 0
  where
    lastPlace' placeUUid = state^?!statePlaceEvents.at placeUUid._Just.placeEventTo

findOtherInterestedPlaces :: UUid URL -> (State, FrontendReplies) -> (State, FrontendReplies)
findOtherInterestedPlaces uuid (state, xs@(_, res):_) = (state, [readyReplyForPlace res state uuid, xs])
findOtherInterestedPlaces _ fr = fr

readyReplyForPlace :: FrontendReply -> State -> UUid URL -> (ConnectedUsers, FrontendReply)
readyReplyForPlace reply state uuid = (users, reply')
  where
    place = state^?!statePlaces.at uuid._Just
    users = place^.placeUsers
    req' = placeFrontendMsg place
    reply' = reply & pePayload . req .~ req'

readyReply :: PlaceEvent -> State -> (State, FrontendReplies)
readyReply (PlaceEvent time' userUUid' placeUUid' previousPlace') state = (state, [(users, frontendReply)])
  where
    users = state^?!statePlaces.at placeUUid'._Just.placeUsers
    name = state^?!stateUsers.at userUUid'._Just.userName
    at' = placeFrontendMsg $ state^?!statePlaces. at placeUUid'._Just
    from' = fmap (placeFrontendMsg . (\u -> state^?!statePlaces. at u._Just)) previousPlace'
    frontendReply = replyPageEvent at' from' at' time' name

propagatePlaceEvent :: PlaceEvent -> State -> State
propagatePlaceEvent placeEvent =
  maybe id (updatePlace False) (placeEvent^.placeEventFrom)
  . updatePlace True (placeEvent^.placeEventTo)
  . updateUserHistory
  . updatePlaceEvents
  where
    userUUid = placeEvent^.placeEventWho
    placeEventUUid = getUUid placeEvent
    updatePlaceEvents = statePlaceEvents.at placeEventUUid?~placeEvent
    updateUserHistory = stateUsers.at userUUid._Just
      %~ userHistory %~ cons placeEventUUid
    updatePlace isUserThere placeUUid = statePlaces.at placeUUid._Just
      %~ (placeHistory %~ cons placeEventUUid) . (placeUsers.contains userUUid.~isUserThere)

ensurePlaceExists :: URL -> Title -> State -> State
ensurePlaceExists url' title' state = case getStatePlace state of
    Just _ -> state
    Nothing -> (setStatePlace $ Place title' url' S.empty []) state
    where
      uuid = getUUid url'
      getStatePlace = (^. statePlaces . at uuid)
      setStatePlace = (statePlaces . at uuid ?~)
