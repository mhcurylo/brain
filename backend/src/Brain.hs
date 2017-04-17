{-# LANGUAGE OverloadedStrings #-}

module Brain
    ( runBrain
    ) where

import BrainData
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

runBrain :: Int -> IO ()
runBrain port = Warp.run port app

app :: Wai.Application
app = WS.websocketsOr WS.defaultConnectionOptions wsApp backupApp
    where
      wsApp :: WS.ServerApp
      wsApp pending = do
        conn <- WS.acceptRequest pending
        WS.forkPingThread conn 30
        msg <- WS.receiveData conn :: IO T.Text
        case msg of
          "Hello Brain!" -> connectWithBrain conn
          _ -> WS.sendTextData conn ("You do not know us." :: T.Text)

connectWithBrain :: WS.Connection -> IO ()
connectWithBrain conn = do
  WS.sendTextData conn ("YO?" :: T.Text)

backupApp :: Wai.Application
backupApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a WebSocket request"

