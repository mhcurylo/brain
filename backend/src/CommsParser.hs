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
parseEventMsg = frontendMsgToEventMsg <=< parseFrontendMsg

frontendMsgToEventMsg :: FrontendMsg -> Maybe EventMsg
frontendMsgToEventMsg (FrontendMsg u t) = case URI.parseURI URI.strictURIParserOptions $ TE.encodeUtf8 u of
   Right uri -> Just $ EventMsg $ URL (normalizeURI uri) $ Title t
   Left _ -> Nothing

normalizeURI :: URI.URIRef URI.Absolute -> B.ByteString
normalizeURI = URI.normalizeURIRef' URI.aggressiveNormalization

parseFrontendMsg :: B.ByteString -> Maybe FrontendMsg
parseFrontendMsg = A.decodeStrict
