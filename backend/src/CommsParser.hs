module CommsParser (
    parseEventMsg
) where

import BrainData
import Control.Monad
import Control.Lens (set, (.~))
import qualified Data.ByteString     as B
import qualified Data.Text.Encoding  as TE
import qualified URI.ByteString      as URI
import qualified Data.Aeson          as A

parseEventMsg :: B.ByteString -> Maybe EventMsg
parseEventMsg = frontendMsgToEventMsg <=< parseFrontendMsg

frontendMsgToEventMsg :: FrontendMsg -> Maybe EventMsg
frontendMsgToEventMsg (FrontendMsg u t) = case URI.parseURI URI.strictURIParserOptions $ TE.encodeUtf8 u of
   Right uri -> Just $ EventMsg  (normalizeURI uri) (Title t)
   Left _ -> Nothing


emptyQuery :: URI.Query
emptyQuery = URI.Query []

cannonicalForm :: URI.URIRef URI.Absolute -> URI.URIRef URI.Absolute
cannonicalForm = (URI.queryL.~emptyQuery) . (URI.fragmentL.~Nothing)

normalizeURI :: URI.URIRef URI.Absolute -> URL
normalizeURI = URL . URI.normalizeURIRef' URI.aggressiveNormalization . cannonicalForm

parseFrontendMsg :: B.ByteString -> Maybe FrontendMsg
parseFrontendMsg = A.decodeStrict
