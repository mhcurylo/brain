module CommsParser (
    parseEventMsg
  , EventMsg(..)
) where

import BrainData
import qualified Data.ByteString                as B
import qualified URI.ByteString                 as URI
import qualified Data.Aeson                     as A

parseEventMsg :: B.ByteString -> IO B.ByteString
parseEventMsg = return
