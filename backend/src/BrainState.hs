module BrainState (
  initState,
  clientPresent,
  addClientToState,
  removeClientFromState
  ) where

import           BrainData
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Network.WebSockets  as WS

initState :: State
initState = State S.empty M.empty M.empty M.empty HM.empty

isNameInUse :: T.Text -> State -> Bool
isNameInUse  name state = S.member name $ stateNamesInUse state

addUser :: UserUUID -> T.Text -> Users -> Users
addUser uuid name = M.insert uuid $ User name $ History [] uuid

removeClientFromState :: UserUUID -> State -> State
removeClientFromState userUUID (State p n u c e) = State p n u (M.delete userUUID c) e

addClientToState :: UserUUID -> T.Text -> WS.Connection -> State -> State
addClientToState userUUID name conn (State p n u c e) = State p (S.insert name n) (addUser userUUID name u) (M.insert userUUID conn c)  h

