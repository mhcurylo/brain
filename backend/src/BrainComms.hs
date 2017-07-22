module BrainComms (
  addUserToComms,
  removeUserFromComms
) where

import           BrainData
import qualified Data.Map            as M
import qualified Network.WebSockets  as WS

addUserToComms :: UUid User -> WS.Connection -> Connections -> Connections
addUserToComms = M.insert

removeUserFromComms :: UUid User -> Connections -> Connections
removeUserFromComms = M.delete
