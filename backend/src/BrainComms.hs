module BrainComms (
  addUserToComms,
  removeUserFromComms
) where

import           BrainData
import qualified Data.Map            as M
import qualified Network.WebSockets  as WS

addUserToComms :: UserUUID -> WS.Connection -> Connections -> Connections
addUserToComms = M.insert

removeUserFromComms :: UserUUID -> Connections -> Connections
removeUserFromComms = M.delete
