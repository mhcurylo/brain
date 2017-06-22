module BrainData (
  Title,
  User,
  Name,
  Time,
  UrlPath,
  Names,
  Connections,
  Places,
  NamesInUse,
  History,
  PlaceEvent(..),
  Place(..),
  State(..),
  MState
                 ) where

import qualified Data.Map           as M
import qualified Data.Set           as S
import qualified Data.Text          as T
import qualified Network.WebSockets as WS
import qualified Data.UUID          as U
import Control.Concurrent (MVar)


type Title = T.Text
type User = U.UUID
type Name = T.Text
type Time = Integer
type UrlPath = [T.Text]

type Names = M.Map User Name
type Connections = M.Map User WS.Connection
type Places = M.Map UrlPath Place
type NamesInUse = S.Set Name
type History = [PlaceEvent]

data PlaceEvent = PlaceEvent {
    placeEventWhen  :: Time
  , placeEventUrl :: UrlPath
  , placeEventWho :: User
}


data Place = Place {
    placeTitle :: Title
  , placeHistory :: History
}

data State = State {
    statePlaces :: Places
  , stateNames :: Names
  , stateNamesInUse :: NamesInUse
  , stateConnections :: Connections
  , stateHistory :: History
}

type MState = MVar State
