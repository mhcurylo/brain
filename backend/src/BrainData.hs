module BrainData (
    Title
  , Url
  , Name
  , Time
  , PlaceData(..)
  , PlaceEvent(..)
  , Path
  , History(..)
  , Histories
  , Story(..)
  , Stories
  , Client(..)
  , Clients
  , Names
  , Connection(..)
  , Connections
  , Place(..)
  , Places
  , State(..)
  , MState
                 ) where

import qualified Data.Map           as M
import qualified Data.Set           as S
import qualified Data.Text          as T
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar)

type Title = T.Text
type Url = T.Text
type Name = T.Text
type Time = Integer

data PlaceData = PlaceData {
    placeUrl   :: Url
  , placeTitle :: Title
}

data PlaceEvent = PlaceEvent {
    placeEventWhen  :: Time
  , placeEventWhere :: PlaceData
}

type Path = [PlaceEvent]

data History = History {
    clientName :: Name
  , clientPath :: Path
}

type Histories = [History]

data Story = Story {
    shortStoryWho  :: Name
  , shortStoryWhen :: Time
  , shortStoryFrom :: PlaceData
  , shortStoryTo   :: PlaceData
}

type Stories = [Story]

data Client = Client {
    clientConnection :: WS.Connection
  , clientHistory    :: History
}

type Clients = M.Map Name Client
type Names = S.Set Name

data Connection = Connection {
    connectionTo       :: PlaceData
  , connectionFrom     :: PlaceData
  , connectionStrength :: Integer
}

type Connections = [Connection]

data Place = Place {
     placeData       :: PlaceData
   , placeNeighbours :: Connections
   , placeStories    :: Stories
   , placeClients    :: Names
}

type Places = M.Map Url Place

data State = State {
    statePlaces :: Places
  , stateClients :: Clients
  , stateHistories :: Histories
}

type MState = MVar State