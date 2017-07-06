module BrainState (
  initState,
  isNameInUse,
  addUserToState,
  removeUserFromState
  ) where

import           BrainData
import qualified Data.Text           as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Network.WebSockets  as WS

initState :: State
initState = State S.empty M.empty M.empty M.empty M.empty

andIsNotEmpty :: T.Text -> Bool -> Bool
andIsNotEmpty = (&&) . not . T.null

isNameInUse :: Name -> State -> Bool
isNameInUse  name state = andIsNotEmpty name . S.member name $ stateNamesInUse state

addUser :: UserUUID -> Name -> Users -> Users
addUser uuid name = M.insert uuid $ User name [] uuid

removeUserFromState :: UserUUID -> Name -> State -> State
removeUserFromState uuid name (State n u c e p) = State (S.delete name n) u (M.delete uuid c) e p

addUserToState :: UserUUID -> Name -> WS.Connection -> State -> State
addUserToState uuid name conn (State n u c e p) = State (S.insert name n) (addUser uuid name u) (M.insert uuid conn c)  e p

