module BrainState where

import           BrainData
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.ByteString     as B
import qualified Data.UUID.V5        as U5
import Control.Lens.At
import Control.Lens
import Data.Maybe

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

addEventToState :: EventData -> State -> (State, ([UUid User], FrontendReply))
addEventToState event state = (\s -> (s, ([], FrontendReply)))
  . propagatePlaceEvent (PlaceEvent time userUUid placeUUid previousPlace)
  . ensurePlaceExists url' title' $ state
  where
      eventMsg = event^.eventDataEventMsg
      time = event^.eventDataTime
      userUUid = event^.eventDataUserUUid
      title' = eventMsg^.eventMsgTitle
      url' = eventMsg^.eventMsgUrl
      placeUUid = getUUid url'
      previousPlace = lastVisited userUUid state

lastVisited :: UUid User -> State -> Maybe (UUid URL)
lastVisited userUUid state = lastPlace' <$> state^?stateUsers.at userUUid._Just.userHistory.ix 0
  where
    lastPlace' placeUUid = state^?!statePlaceEvents.at placeUUid._Just.placeEventFrom._Just


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
    updateUserHistory = stateUsers.at userUUid._Just %~ userHistory %~ cons placeEventUUid
    updatePlace isUserThere placeUUid = statePlaces.at placeUUid._Just %~ (placeHistory %~ cons placeEventUUid) . (placeUsers . contains userUUid .~ isUserThere)

ensurePlaceExists :: URL -> Title -> State -> State
ensurePlaceExists url' title' state = case getStatePlace state of
    Just _ -> state
    Nothing -> (setStatePlace $ Place title' url' S.empty []) state
    where
      uuid = getUUid url'
      getStatePlace = (^. statePlaces . at uuid)
      setStatePlace = (statePlaces . at uuid ?~)
