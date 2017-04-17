module BrainState (
  initState,
  clientPresent,
  addClientToState
  ) where

import           BrainData
import qualified Data.Map           as M
import qualified Data.Text          as T
import qualified Network.WebSockets as WS

initState :: State
initState = State M.empty M.empty []

clientPresent :: T.Text -> State -> Bool
clientPresent name state = M.member name $ stateClients state

addClient :: T.Text -> WS.Connection -> Clients -> Clients
addClient name conn = M.insert name $ Client conn $ History name []

removeClientFromState :: T.Text -> State -> State
removeClientFromState name (State p c h)= State p (M.delete name c) h

addClientToState :: T.Text -> WS.Connection -> State -> State
addClientToState name conn (State p c h) = State p (addClient name conn c) h

