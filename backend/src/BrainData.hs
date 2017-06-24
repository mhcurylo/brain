{-# LANGUAGE DeriveGeneric #-}

module BrainData (
  Title,
  UserUUID,
  Name,
  Time,
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
import qualified Data.Hashable       as H
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Network.WebSockets  as WS
import qualified Data.UUID           as U
import Control.Concurrent (MVar)


type Title = T.Text
type UserUUID = U.UUID
type Name = T.Text
type Time = Integer
type UrlPath = [T.Text]

type Users = M.Map UserUUID User
type Connections = M.Map UserUUID WS.Connection
type Places = HM.HashMap UrlPath Place
type NamesInUse = S.Set Name
type PlaceEvents = HM.HashMap PlaceEvent PlaceEvent
type History = [Int]

data User = User {
    userName :: Name
  , userHistory :: History
} deriving Show

data PlaceEvent = PlaceEvent {
    placeEventWhen  :: Time
  , placeEventUserUUID :: UserUUID
  , placeEventUserName :: Name
  , placeEventTo :: UrlPath
  , placeEventFrom :: UrlPath
} deriving (Show, Generic)

data Place = Place {
    placeTitle :: Title
  , placeUrl :: UrlPath
  , placeHistory :: History
} deriving (Show)

data State = State {
    statePlaces :: Places
  , stateUsers :: Users
  , stateNamesInUse :: NamesInUse
  , stateConnections :: Connections
  , statePlaceEvents :: PlaceEvents
}

type MState = MVar State
