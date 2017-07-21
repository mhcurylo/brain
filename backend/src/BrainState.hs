module BrainState where

import           BrainData
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.UUID.V5        as U5
import qualified Data.ByteString     as B
import Control.Lens.At
import Control.Lens
import Data.Maybe

initState :: State
initState = State S.empty M.empty M.empty M.empty

initComms :: Connections
initComms = M.empty

isNameInUse :: Name -> State -> Bool
isNameInUse  name = (^. stateNamesInUse . contains name)

freshUser :: UserUUID -> Name -> State -> State
freshUser uuid name = stateUsers . at uuid ?~ User name [] uuid

removeUserFromState :: UserUUID -> Name -> State -> State
removeUserFromState uuid name = (stateNamesInUse . contains name).~False

addUserToState :: UserUUID -> Name -> State -> State
addUserToState uuid name = (stateNamesInUse . contains name .~ True) . freshUser uuid name

addEventToState :: EventData -> State -> (State, ([UserUUID], FrontendReply))
addEventToState event = (\s -> (s, ([], FrontendReply))) . propagatePlaceEvent placeEvent . ensurePlaceExists placeUUID url' title'
  where
      eventMsg = event^.eventDataEventMsg
      time = event^.eventDataTime
      title' = eventMsg^.eventMsgTitle
      url' = eventMsg^.eventMsgUrl
      userUUID = event^.eventDataUserUUID
      placeUUID = urlUUID url'
      previousPlace = Just placeUUID
      placeEvent = PlaceEvent time userUUID placeUUID previousPlace

propagatePlaceEvent :: PlaceEvent -> State -> State
propagatePlaceEvent pevent = id


urlUUID :: URL -> UrlUUID
urlUUID (URL u) = U5.generateNamed U5.namespaceURL $ B.unpack u

ensurePlaceExists :: UrlUUID -> URL -> Title -> State -> State
ensurePlaceExists uuid url' title' state = case getStatePlace state of
    Just _ -> state
    Nothing -> (setStatePlace $ Place title' url' S.empty []) state
    where
      getStatePlace = (^. statePlaces . at uuid)
      setStatePlace = (statePlaces . at uuid ?~)
