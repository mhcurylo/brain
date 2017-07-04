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
  name <- addClientToMState conn mstate
  finally (connectUserToBrain name conn mstate) (removeClientFromMState name mstate)

connectUserToBrain :: Name -> System.UUID.V4 WS.Connection -> MState -> IO ()
connectUserToBrain name conn mstate = forever $ do
  msg <- WS.receiveData conn
  WS.sendTextData conn $ T.append "You spoke about " msg;

addClientToMState :: WS.Connection -> MState -> IO Name
addClientToMState conn mstate = do
  name <- runName
  state <- readMVar mstate
  if clientPresent name state
    then addClientToMState conn mstate
    else do
      TIO.putStrLn name
      modifyMVar_ mstate $ return . addClientToState name conn
      return name

removeClientFromMState :: Name -> MState -> IO ()
removeClientFromMState name = flip modifyMVar $ return . removeClientFromState name
