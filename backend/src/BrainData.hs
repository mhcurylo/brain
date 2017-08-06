{-# LANGUAGE TemplateHaskell #-}

module BrainData where

import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.ByteString     as B
import qualified Data.ByteString.Char8 as BChar
import qualified Data.Set            as S
import qualified Network.WebSockets  as WS
import qualified Data.UUID           as U
import qualified Data.Aeson          as A
import qualified Data.Aeson.TH       as A
import qualified Data.Time.Clock     as TC
import qualified Data.UUID.V5        as U5
import Data.Maybe (fromJust)
import Control.Lens hiding (at)
import Control.Concurrent (MVar)

namespacePE :: U.UUID
namespacePE = fromJust . U.fromText $ T.pack "66961c15-6ead-11e7-8001-7446-a0bb45b3"

newtype URL = URL B.ByteString deriving (Show, Eq, Ord)
newtype Title = Title T.Text deriving (Show, Eq, Ord)
newtype Name = Name B.ByteString deriving (Show, Eq, Ord)
type NamesInUse = S.Set Name

newtype UUid a = UUid U.UUID deriving (Show, Eq, Ord)
type History = [UUid PlaceEvent]

data User = User {
    _userName :: Name
  , _userHistory :: History
  , _userUUID :: UUid User
} deriving (Show, Eq, Ord)

data PlaceEvent = PlaceEvent {
    _placeEventWhen  :: TC.UTCTime
  , _placeEventWho :: UUid User
  , _placeEventTo :: UUid URL
  , _placeEventFrom :: Maybe (UUid URL)
} deriving (Show, Eq, Ord)

makeLenses ''PlaceEvent
makeLenses ''User

class GenUUid a where
  getUUid :: a -> UUid a

instance GenUUid URL where
  getUUid = UUid . U5.generateNamed U5.namespaceURL . B.unpack . (\(URL u) -> u)

instance GenUUid PlaceEvent where
  getUUid = UUid . U5.generateNamed U5.namespaceURL . B.unpack . BChar.pack . show

type Users = M.Map (UUid User) User
type ConnectedUsers = S.Set (UUid User)
type Connections = M.Map (UUid User) WS.Connection

type PlaceEvents = M.Map (UUid PlaceEvent) PlaceEvent

data Place = Place {
    _placeTitle :: Title
  , _placeUrl :: URL
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
    _eventDataUserUUid     :: UUid User
  , _eventDataEventMsg :: EventMsg
  , _eventDataTime     :: TC.UTCTime
} deriving (Show, Eq, Ord)
makeLenses ''EventData

data FrontendMsg = FrontendMsg {
    _url :: T.Text
  , _title :: T.Text
} deriving (Show, Eq, Ord)
makeLenses ''FrontendMsg
A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1} ''FrontendMsg

data ACTION_KIND = PAGE_EVENT_ACTION | CANONICAL_URL_ACTION deriving (Show, Eq, Ord);
A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1} ''ACTION_KIND

data ActionPayload = PageEventPayload {
    _at :: FrontendMsg
  , _from :: Maybe FrontendMsg
  , _req :: FrontendMsg
  , _when :: T.Text
  , _who :: T.Text
  } | CanonicalActionPayload {
    _original :: T.Text
  , _canonical :: T.Text
  } deriving (Show, Eq, Ord);
makeLenses ''ActionPayload
A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1, A.sumEncoding = A.UntaggedValue} ''ActionPayload

data FrontendReply = FrontendReply {
    _kind :: ACTION_KIND
  , _payload :: ActionPayload
} deriving (Show, Eq, Ord)
makeLenses ''FrontendReply
A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1} ''FrontendReply

placeFrontendMsg :: Place -> FrontendMsg
placeFrontendMsg (Place (Title t) (URL u) _ _) = FrontendMsg (T.decodeUtf8 u) t

type FrontendReplies = [(ConnectedUsers, FrontendReply)]
