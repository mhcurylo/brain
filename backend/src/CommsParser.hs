module CommsParser (
    parseEventMsg
  , EventMsg(..)
) where

import BrainData
import qualified Data.Text                      as T
import qualified URI.ByteString                 as URI
import qualified Data.Aeson                     as A

parseEventMsg :: T.Text -> IO T.Text
parseEventMsg = return
