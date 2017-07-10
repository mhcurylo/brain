module CommsParser (
    parseEventMsg
  , EventMsg(..)
) where

import BrainData
import qualified Data.Text                      as T

data EventMsg = EventMsg {
    eventMsgUrlPath :: UrlPath
  , eventMsgTitle :: Title
}

data FrontendMsg = FrontendMsg {
    url :: T.Text
  , title :: T.Text
}

parseEventMsg :: T.Text -> T.Text
parseEventMsg = id
