{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module BrainData where

import qualified Data.Aeson          as A
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.ByteString     as B
import qualified Data.Set            as S
import qualified Network.WebSockets  as WS
import qualified Data.UUID           as U
import qualified Data.Time.Clock     as TC
import qualified Data.UUID.V5        as U5
import qualified Data.Word           as W
import Data.Maybe (fromJust)
import Control.Lens hiding (at)
import Control.Concurrent (MVar)

namespacePE :: U.UUID
namespacePE = fromJust . U.fromText $ T.pack "66961c15-6ead-11e7-8001-7446-a0bb45b3"

unpackText :: T.Text -> [W.Word8]
unpackText = B.unpack . T.encodeUtf8

newtype URL = URL { _uRL :: T.Text } deriving (Show, Eq, Ord, A.FromJSON, A.ToJSON)
makeLenses ''URL
newtype Title = Title {_tITLE :: T.Text } deriving (Show, Eq, Ord, A.FromJSON, A.ToJSON)
makeLenses ''Title
newtype Name = Name {_nAME :: T.Text} deriving (Show, Eq, Ord, A.FromJSON, A.ToJSON)
makeLenses ''Name
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
  getUUid = UUid . U5.generateNamed U5.namespaceURL . unpackText . (\(URL u) -> u)

instance GenUUid PlaceEvent where
  getUUid = UUid . U5.generateNamed namespacePE . unpackText . T.pack . show

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
