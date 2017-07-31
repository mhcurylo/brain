{-# LANGUAGE OverloadedStrings #-}

module Brain
    ( runBrain
    ) where

import BrainData
import BrainState
import NameGen
import BrainComms
import CommsParser (parseEventMsg)
import qualified Data.Aeson                     as A
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as BChar
import qualified Data.Map                       as M
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Data.UUID.V4                   as U4
import qualified Data.Time.Clock                as TC
import Data.Maybe (isJust, fromJust)
import Control.Exception (finally)
import Control.Monad (forever, forM_)
import Control.Concurrent (newMVar, modifyMVar, modifyMVar_, readMVar)

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
          "Hello Brain!" -> handshake conn mstate mcomms
          _ -> WS.sendTextData conn ("Wrong handshake." :: B.ByteString)

handshake :: WS.Connection -> MState -> MComms -> IO ()
handshake conn mstate mcomms = do
  (name, uuid) <- addUserToMState conn mstate mcomms
  finally (connection uuid conn mstate mcomms) (removeUserFromMState uuid name mstate mcomms)

connection :: UUid User -> WS.Connection -> MState -> MComms -> IO ()
connection uuid conn mstate mcomms = forever $ do
  msg <- WS.receiveData conn
  time <- TC.getCurrentTime
  let msg' = parseEventMsg msg
  if isJust msg'
    then communicateEvent mstate mcomms $ EventData uuid (fromJust msg') time
    else print $ "Error in parsing" ++ show msg

communicateEvent :: MState -> MComms -> EventData -> IO ()
communicateEvent mstate mcomms event = do
  frontendReplies <- addEventToMState mstate event
  forM_ frontendReplies (sendReplies mcomms)

sendReplies :: MComms -> (ConnectedUsers, FrontendReply) -> IO ()
sendReplies mcomms (users, reply) = do
  comms <- readMVar mcomms
  let repJSON = A.encode reply
  let usersWS = M.elems $ M.intersection comms (M.fromSet id users)
  forM_ usersWS (`WS.sendTextData` repJSON)

addEventToMState :: MState -> EventData -> IO FrontendReplies
addEventToMState mstate event = modifyMVar mstate $ return . addEventToState event

addUserToMState :: WS.Connection -> MState -> MComms -> IO (Name, UUid User)
addUserToMState conn mstate mcomms = do
  name <- runName
  state <- readMVar mstate
  if isNameInUse name state
    then addUserToMState conn mstate mcomms
    else do
      uuid <- UUid <$> U4.nextRandom
      let pname = (\(Name n) -> n) name
      BChar.putStrLn pname
      modifyMVar_ mstate $ return . addUserToState uuid name
      modifyMVar_ mcomms $ return . addUserToComms uuid conn
      return (name, uuid)

removeUserFromMState :: UUid User -> Name -> MState -> MComms -> IO ()
removeUserFromMState uuid name mstate mcomms = do
  modifyMVar_ mstate $ return . removeUserFromState uuid name
  modifyMVar_ mcomms $ return . removeUserFromComms uuid
