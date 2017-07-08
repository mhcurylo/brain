{-# LANGUAGE DeriveGeneric #-}

module BrainData (
  Title,
  UserUUID(..),
  Users,
  Name(..),
  UrlPath,
  Connections,
  Places,
  NamesInUse,
  History,
  PlaceEvent(..),
  Place(..),
  State(..),
  User(..),
  MComms,
  MState
  ) where

import GHC.Generics (Generic)
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Network.WebSockets  as WS
import qualified Data.UUID           as U
import qualified Data.Time.Clock     as TC
import Control.Concurrent (MVar)
import Data.Word (Word32)
import Test.QuickCheck (Gen, Arbitrary, listOf1, arbitrary)

instance Arbitrary Name where
  arbitrary = do
    text <- listOf1 arbitrary
    return $ Name $ T.pack text

instance Arbitrary UserUUID where
  arbitrary = do
    w1 <- arbitrary :: Gen Word32
    w2 <- arbitrary :: Gen Word32
    w3 <- arbitrary :: Gen Word32
    w4 <- arbitrary :: Gen Word32
    return $ UserUUID $ U.fromWords w1 w2 w3 w4

type Title = T.Text
newtype UserUUID = UserUUID U.UUID deriving (Show, Eq, Ord)
type PlaceEventUUID = U.UUID
newtype Name = Name T.Text deriving (Show, Eq, Ord)
type UrlPath = T.Text
type UrlUUID = U.UUID
type History = [PlaceEventUUID]
type Places = M.Map UrlUUID Place
type NamesInUse = S.Set Name
type Users = M.Map UserUUID User
type PlaceEvents = M.Map PlaceEventUUID PlaceEvent
type Connections = M.Map UserUUID WS.Connection

data User = User {
    userName :: Name
  , userHistory :: History
  , userUUID :: UserUUID
  , userConnected :: Bool
} deriving (Show)

data PlaceEvent = PlaceEvent {
  placeEventWhen  :: TC.UTCTime
  , placeEventUserUUID :: UserUUID
  , placeEventTo :: UrlUUID
  , placeEventFrom :: UrlUUID
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
  , statePlaceEvents :: PlaceEvents
  , statePlaces :: Places
} deriving (Show)

instance Arbitrary State where
  arbitrary = do
    let namesInUse = S.empty
    let users = M.empty
    let placeEvents = M.empty
    let places = M.empty
    return $ State namesInUse users placeEvents places

type MState = MVar State
type MComms = MVar Connections
