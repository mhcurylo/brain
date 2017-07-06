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
import qualified Data.UUID.V4        as U4
import qualified Data.Time.Clock     as TC
import qualified Data.Time.Calendar  as TCA
import Control.Concurrent (MVar)
import Data.Word (Word32)
import Test.QuickCheck (Gen, Arbitrary, arbitrary, choose)

instance Arbitrary T.Text where
  arbitrary = do
    text <- arbitrary
    return $ T.pack text

instance Arbitrary U.UUID where
  arbitrary = do
    w1 <- arbitrary :: Gen Word32
    w2 <- arbitrary :: Gen Word32
    w3 <- arbitrary :: Gen Word32
    w4 <- arbitrary :: Gen Word32
    return $ U.fromWords w1 w2 w3 w4

instance Arbitrary TC.UTCTime where
  arbitrary = do
    randomDay <- choose (1, 29) :: Gen Int
    randomMonth <- choose (1, 12) :: Gen Int
    randomYear <- choose (2001, 2002) :: Gen Integer
    randomTime <- choose (0, 86401) :: Gen Int
    return $ TC.UTCTime (TCA.fromGregorian randomYear randomMonth randomDay) (fromIntegral randomTime)

type Title = T.Text
type UserUUID = U.UUID
type PlaceEventUUID = U.UUID
type Name = T.Text
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
  , stateConnections :: Connections
  , statePlaceEvents :: PlaceEvents
  , statePlaces :: Places
}

instance Arbitrary State where
  arbitrary = do
    namesInUse <- arbitrary
    let users = M.empty
    let conns = M.empty
    let placeEvents = M.empty
    let places = M.empty
    return $ State namesInUse users conns placeEvents places

type MState = MVar State
