module CommsParser (
    parseEventMsg
) where

import BrainData
import Control.Monad
import qualified Data.ByteString     as B
import qualified Data.Text.Encoding  as TE
import qualified URI.ByteString      as URI
import qualified Data.Aeson          as A

parseEventMsg :: B.ByteString -> Maybe EventMsg
parseEventMsg = eventMsgToFrontendMsg <=< parseFrontendMsg

eventMsgToFrontendMsg :: FrontendMsg -> Maybe EventMsg
eventMsgToFrontendMsg (FrontendMsg u t) = case URI.parseURI URI.strictURIParserOptions$ TE.encodeUtf8 u of
   Right uri -> Just $ EventMsg uri $ Title t
   Left _ -> Nothing

parseFrontendMsg :: B.ByteString -> Maybe FrontendMsg
parseFrontendMsg = A.decodeStrict
