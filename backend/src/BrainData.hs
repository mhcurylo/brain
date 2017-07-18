{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module BrainData where

import GHC.Generics (Generic)
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.ByteString     as B
import qualified Data.Set            as S
import qualified Network.WebSockets  as WS
import qualified Data.UUID           as U
import qualified Data.Aeson          as A
import qualified Data.Time.Clock     as TC
import Control.Lens
import Control.Concurrent (MVar)

newtype URL = URL B.ByteString deriving (Show, Eq, Ord)
newtype Title = Title T.Text deriving (Show, Eq, Ord)
newtype UserUUID = UserUUID U.UUID deriving (Show, Eq, Ord)
type PlaceEventUUID = U.UUID
newtype Name = Name B.ByteString deriving (Show, Eq, Ord)
type UrlPath = B.ByteString
type UrlUUID = U.UUID
type History = [PlaceEventUUID]
type NamesInUse = S.Set Name
type Users = M.Map UserUUID User
type ConnectedUsers = S.Set UserUUID
type Connections = M.Map UserUUID WS.Connection

data User = User {
    _userName :: Name
  , _userHistory :: History
  , _userUUID :: UserUUID
} deriving (Show, Eq, Ord)

makeLenses ''User

data PlaceEvent = PlaceEvent {
  _placeEventWhen  :: TC.UTCTime
  , _placeEventUserUUID :: UserUUID
  , _placeEventTo :: UrlUUID
  , _placeEventFrom :: Maybe UrlUUID
  , _placeEventUUID :: PlaceEventUUID
} deriving (Show, Eq, Ord, Generic)

makeLenses ''PlaceEvent

type PlaceEvents = M.Map PlaceEventUUID PlaceEvent

data Place = Place {
    _placeTitle :: Title
  , _placeUrl :: UrlPath
  , _placeUsers :: ConnectedUsers
  , _placeHistory :: History
} deriving (Show, Eq, Ord)

makeLenses ''Place

type Places = M.Map UrlUUID Place

data State = State {
    _stateNamesInUse :: NamesInUse
  , _stateUsers :: Users
  , _statePlaceEvents :: PlaceEvents
  , _statePlaces :: Places
} deriving (Show, Eq, Ord)

makeLenses ''State

type MState = MVar State
type MComms = MVar Connections

data EventMsg = EventMsg {
    _eventMsgUrl      :: URL
  , _eventMsgTitle    :: Title
} deriving (Show, Eq, Ord)

makeLenses ''EventMsg

data EventData = EventData {
    _eventDataUserUUID :: UserUUID
  , _eventDataEventMsg :: EventMsg
  , _eventDataTime     :: TC.UTCTime
} deriving (Show, Eq, Ord)

makeLenses ''EventData

data FrontendReply = FrontendReply {
} deriving (Show, Eq, Ord, Generic)

data FrontendMsg = FrontendMsg {
    url :: T.Text
  , title :: T.Text
} deriving (Show, Eq, Ord, Generic)

instance A.FromJSON FrontendMsg
