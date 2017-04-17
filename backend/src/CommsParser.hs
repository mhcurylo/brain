module CommsParser (
    parseEventMsg
  , EventMsg(..)
) where

import BrainData
import qualified Data.Text                      as T

data EventMsg = EventMsg {
    eventMsgUrl :: Url
  , eventMsgUrlPath :: UrlPath
  , eventMsgTitle :: Title
}

parseEventMsg :: T.Text -> T.Text
parseEventMsg = id
