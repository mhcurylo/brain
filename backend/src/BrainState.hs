module BrainState (
  initState,
  initComms,
  isNameInUse,
  addUserToState,
  removeUserFromState
  ) where

import           BrainData
import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Network.WebSockets  as WS

initState :: State
initState = State S.empty M.empty M.empty M.empty

initComms :: Connections
initComms = M.empty

isNameInUse :: Name -> State -> Bool
isNameInUse  name state = S.member name $ stateNamesInUse state

addUser :: UserUUID -> Name -> Users -> Users
addUser uuid name = M.insert uuid $ User name [] uuid

removeUserFromState :: UserUUID -> Name -> State -> State
removeUserFromState uuid name (State n u e p) = State (S.delete name n) users e p
  where
    users = M.adjust (\ (User _ history _) -> User name history uuid) uuid u

addUserToState :: UserUUID -> Name -> State -> State
addUserToState uuid name (State n u e p) = State (S.insert name n) (addUser uuid name u) e p

