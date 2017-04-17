{-# LANGUAGE OverloadedStrings #-}

module Brain
    ( runBrain
    ) where

import BrainData
import BrainState
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
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
          _ -> WS.sendTextData conn ("You do not know us." :: T.Text)

connectWithBrain :: WS.Connection -> MState -> IO ()
connectWithBrain conn mstate = do
  addClientToMState conn mstate
  WS.sendTextData conn ("You do know us." :: T.Text)

addClientToMState :: WS.Connection -> MState -> IO ()
addClientToMState conn mstate = do
  name <- getRandomName
  state <- readMVar mstate
  if clientPresent name state
    then addClientToMState conn mstate
    else modifyMVar_ mstate (\s -> return $ addClientToState name conn s)

getRandomName :: IO T.Text
getRandomName = return "boo"
