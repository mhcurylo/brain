{-# LANGUAGE OverloadedStrings #-}

module Brain
    ( runBrain
    ) where

import BrainData
import BrainState
import NameGen
import BrainComms
import CommsParser (parseEventMsg)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as BChar
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Data.UUID.V4                   as U4
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Concurrent (newMVar, modifyMVar_, readMVar)

runBrain :: Int -> IO ()
runBrain port = do
  mstate <- newMVar initState
  mcomms <- newMVar initComms
  Warp.run port $ app mstate mcomms

app :: MState -> MComms -> Wai.Application
app mstate mcomms = WS.websocketsOr WS.defaultConnectionOptions (wsApp mstate mcomms) backupApp

backupApp :: Wai.Application
backupApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a WebSocket request"

wsApp :: MState -> MComms -> WS.ServerApp
wsApp mstate mcomms pending = do
        conn <- WS.acceptRequest pending
        WS.forkPingThread conn 30
        msg <- WS.receiveData conn :: IO B.ByteString
        case msg of
          "Hello Brain!" -> connectWithBrain conn mstate mcomms
          _ -> WS.sendTextData conn ("Wrong handshake." :: B.ByteString)

connectWithBrain :: WS.Connection -> MState -> MComms -> IO ()
connectWithBrain conn mstate mcomms = do
  (name, uuid) <- addUserToMState conn mstate mcomms
  finally (communicateWithBrain uuid name conn mstate mcomms) (removeClientFromMState uuid name mstate mcomms)


communicateWithBrain :: UserUUID -> Name -> WS.Connection -> MState -> MComms -> IO ()
communicateWithBrain uuid name conn mstate mcomms = forever $ do
  msg <- WS.receiveData conn
  print $ parseEventMsg msg
  WS.sendTextData conn $ B.append "You spoke about " msg;

addUserToMState :: WS.Connection -> MState -> MComms -> IO (Name, UserUUID)
addUserToMState conn mstate mcomms = do
  name <- runName
  state <- readMVar mstate
  if isNameInUse name state
    then addUserToMState conn mstate mcomms
    else do
      uuid <- UserUUID <$> U4.nextRandom
      let pname = (\(Name n) -> n) name
      BChar.putStrLn pname
      modifyMVar_ mstate $ return . addUserToState uuid name
      modifyMVar_ mcomms $ return . addUserToComms uuid conn
      return (name, uuid)

removeClientFromMState :: UserUUID -> Name -> MState -> MComms -> IO ()
removeClientFromMState uuid name mstate mcomms = do
  modifyMVar_ mstate $ return . removeUserFromState uuid name
  modifyMVar_ mcomms $ return . removeUserFromComms uuid
