{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Lib
import qualified Data.Text                      as T
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

main :: IO ()
main = Warp.run 3000 app

app :: Wai.Application
app = WS.websocketsOr WS.defaultConnectionOptions wsApp backupApp
    where
      wsApp :: WS.ServerApp
      wsApp pending_conn = do
        conn <- WS.acceptRequest pending_conn
        print "Connected"

backupApp :: Wai.Application
backupApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a WebSocket request"
