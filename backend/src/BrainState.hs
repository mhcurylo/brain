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
addEventToState event state = (\s -> (s, ([], FrontendReply))) . propagatePlaceEvent placeEvent . ensurePlaceExists url' title' $ state
  where
      eventMsg = event^.eventDataEventMsg
      time = event^.eventDataTime
      title' = eventMsg^.eventMsgTitle
      url' = eventMsg^.eventMsgUrl
      userUUID' = event^.eventDataUUid User
      placeUUID = getUUid url'
      previousEvent = state^?stateUsers.at userUUID'._Just.userHistory.ix 0
      previousPlace = state^?statePlaceEvents.at previousEvent._Just.placeEventFrom
      placeEvent = PlaceEvent time userUUID' placeUUID previousPlace

propagatePlaceEvent :: PlaceEvent -> State -> State
propagatePlaceEvent placeEvent = id

ensurePlaceExists :: URL -> Title -> State -> State
ensurePlaceExists url' title' state = case getStatePlace state of
    Just _ -> state
    Nothing -> (setStatePlace $ Place title' url' S.empty []) state
    where
      uuid = getUUid url'
      getStatePlace = (^. statePlaces . at uuid)
      setStatePlace = (statePlaces . at uuid ?~)
