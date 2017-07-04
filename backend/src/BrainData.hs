{-# LANGUAGE DeriveGeneric #-}

module BrainData (
  Title,
  UserUUID,
  Users,
  Name,
  UrlPath,
  Connections,
  Places,
  NamesInUse,
  History,
  PlaceEvent(..),
  Place(..),
  State(..),
  User(..),
  MState
  ) where

import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Network.WebSockets  as WS
import qualified Data.UUID           as U
import qualified Data.Time.Clock     as TC
import Control.Concurrent (MVar)

type Title = T.Text
type UserUUID = U.UUID
type PlaceEventUUID = U.UUID
type Name = T.Text
type UrlPath = [T.Text]
type History = [PlaceEventUUID]

type Places = HM.HashMap UrlPath Place
type NamesInUse = S.Set Name
type Users = M.Map UserUUID User
type PlaceEvents = M.Map PlaceEventUUID PlaceEvent
type Connections = M.Map UserUUID WS.Connection

data User = User {
    userName :: Name
  , userHistory :: History
  , userUUID :: UserUUID
} deriving Show

data PlaceEvent = PlaceEvent {
    placeEventWhen  :: TC.UTCTime
  , placeEventUserUUID :: UserUUID
  , placeEventTo :: UrlPath
  , placeEventFrom :: UrlPath
  , placeEventUUID :: PlaceEventUUID
} deriving (Show, Generic)

data Place = Place {
    placeTitle :: Title
  , placeUrl :: UrlPath
  , placeHistory :: History
} deriving (Show)

data State = State {
    stateNamesInUse :: NamesInUse
  , stateUsers :: Users
  , stateConnections :: Connections
  , statePlaceEvents :: PlaceEvents
  , statePlaces :: Places
}

type MState = MVar State
