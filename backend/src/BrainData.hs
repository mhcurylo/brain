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
type Places = M.Map UrlUUID Place
type NamesInUse = S.Set Name
type Users = M.Map UserUUID User
type ConnectedUsers = S.Set UserUUID
type PlaceEvents = M.Map PlaceEventUUID PlaceEvent
type Connections = M.Map UserUUID WS.Connection

data User = User {
    userName :: Name
  , userHistory :: History
  , userUUID :: UserUUID
} deriving (Show, Eq, Ord)

data PlaceEvent = PlaceEvent {
  placeEventWhen  :: TC.UTCTime
  , placeEventUserUUID :: UserUUID
  , placeEventTo :: UrlUUID
  , placeEventFrom :: Maybe UrlUUID
  , placeEventUUID :: PlaceEventUUID
} deriving (Show, Eq, Ord, Generic)

data Place = Place {
    placeTitle :: Title
  , placeUrl :: UrlPath
  , placeUsers :: ConnectedUsers
  , placeHistory :: History
} deriving (Show, Eq, Ord)

data State = State {
    stateNamesInUse :: NamesInUse
  , stateUsers :: Users
  , statePlaceEvents :: PlaceEvents
  , statePlaces :: Places
} deriving (Show, Eq, Ord)

type MState = MVar State
type MComms = MVar Connections

data EventMsg = EventMsg {
    eventMsgUrl      :: URL
  , eventMsgTitle    :: Title
} deriving (Show, Eq, Ord)

data EventData = EventData {
    eventDataUserUUID :: UserUUID
  , eventDataEventMsg :: EventMsg
  , eventDataTime     :: TC.UTCTime
} deriving (Show, Eq, Ord)

data FrontendReply = FrontendReply {
} deriving (Show, Eq, Ord, Generic)

data FrontendMsg = FrontendMsg {
    url :: T.Text
  , title :: T.Text
} deriving (Show, Eq, Ord, Generic)

instance A.FromJSON FrontendMsg
