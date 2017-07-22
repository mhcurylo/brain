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
import qualified Data.UUID.V5        as U5
import Data.Maybe (fromJust)
import Control.Lens
import Control.Concurrent (MVar)

namespacePE :: U.UUID
namespacePE = fromJust . U.fromText $ T.pack "66961c15-6ead-11e7-8001-7446-a0bb45b3"

newtype URL = URL B.ByteString deriving (Show, Eq, Ord)
newtype Title = Title T.Text deriving (Show, Eq, Ord)
newtype Name = Name B.ByteString deriving (Show, Eq, Ord)
type UrlPath = B.ByteString
type NamesInUse = S.Set Name

newtype UUid a = UUid U.UUID deriving (Show, Eq, Ord)

class GenUUid a where
  getUUid :: a -> UUid a

instance GenUUid URL where
  getUUid = UUid . U5.generateNamed U5.namespaceURL . B.unpack . (\(URL u) -> u)

data PlaceEvent = PlaceEvent {
  _placeEventWhen  :: TC.UTCTime
  , _placeEventUUid :: UUid PlaceEvent
  , _placeEventTo :: UUid URL
  , _placeEventFrom :: Maybe (UUid URL)
} deriving (Show, Eq, Ord, Generic)

makeLenses ''PlaceEvent
type History = [UUid PlaceEvent]

data User = User {
    _userName :: Name
  , _userHistory :: History
  , _userUUID :: UUid User
} deriving (Show, Eq, Ord)
makeLenses ''User

type Users = M.Map (UUid User) User
type ConnectedUsers = S.Set (UUid User)
type Connections = M.Map (UUid User) WS.Connection

type PlaceEvents = M.Map (UUid PlaceEvent) PlaceEvent

data Place = Place {
    _placeTitle :: Title
  , _placeUrl ::URL
  , _placeUsers :: ConnectedUsers
  , _placeHistory :: History
} deriving (Show, Eq, Ord)

makeLenses ''Place

type Places = M.Map (UUid URL) Place

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
    _eventDataUUid     :: UUid User
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
