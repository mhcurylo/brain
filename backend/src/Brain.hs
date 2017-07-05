{-# LANGUAGE OverloadedStrings #-}

module Brain
    ( runBrain
    ) where

import BrainData
import BrainState
import NameGen
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Data.UUID.V4                   as U4
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import qualified Data.Map           as M
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

runBrain :: Int -> IO ()
runBrain port = do
  mstate <- newMVar initState
  Warp.run port $ app mstate

app :: MState -> Wai.Application
app mstate = WS.websocketsOr WS.defaultConnectionOptions (wsApp mstate) backupApp

backupApp :: Wai.Application
backupApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a WebSocket request"

wsApp :: MState -> WS.ServerApp
wsApp mstate pending = do
        conn <- WS.acceptRequest pending
        WS.forkPingThread conn 30
        msg <- WS.receiveData conn :: IO T.Text
        case msg of
          "Hello Brain!" -> connectWithBrain conn mstate
          _ -> WS.sendTextData conn ("Wrong handshake." :: T.Text)

connectWithBrain :: WS.Connection -> MState -> IO ()
connectWithBrain conn mstate = do
  (name, uuid) <- addUserToMState conn mstate
  finally (connectUserToBrain uuid name conn mstate) (removeClientFromMState uuid name mstate)

connectUserToBrain :: UserUUID -> Name -> WS.Connection -> MState -> IO ()
connectUserToBrain uuid name conn mstate = forever $ do
  msg <- WS.receiveData conn
  WS.sendTextData conn $ T.append "You spoke about " msg;

addUserToMState :: WS.Connection -> MState -> IO (Name, UserUUID)
addUserToMState conn mstate = do
  name <- runName
  state <- readMVar mstate
  if isNameInUse name state
    then addUserToMState conn mstate
    else do
      uuid <- U4.nextRandom
      TIO.putStrLn name
      modifyMVar_ mstate $ return . addUserToState uuid name conn
      return (name, uuid)

removeClientFromMState :: UserUUID -> Name -> MState -> IO ()
removeClientFromMState uuid name = flip modifyMVar_ $ return . removeUserFromState uuid name
