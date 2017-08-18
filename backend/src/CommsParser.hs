module CommsParser where

import BrainData
import BrainMsg
import Control.Lens ((.~), (^.), over)
import qualified Data.ByteString              as B
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy               as TL
import qualified URI.ByteString               as URI
import qualified Data.Aeson                   as A
import qualified Text.Blaze                   as BL
import qualified Text.Blaze.Renderer.Text     as BL


emptyQuery :: URI.Query
emptyQuery = URI.Query []

canonicalForm :: URI.URIRef URI.Absolute -> URI.URIRef URI.Absolute
canonicalForm = (URI.queryL.~emptyQuery) . (URI.fragmentL.~Nothing)

normalizeURI :: URI.URIRef URI.Absolute -> T.Text
normalizeURI = T.decodeUtf8 . URI.normalizeURIRef' URI.aggressiveNormalization . canonicalForm

normalizeTextURI :: T.Text -> Either URI.URIParseError T.Text
normalizeTextURI = fmap normalizeURI . URI.parseURI URI.strictURIParserOptions . T.encodeUtf8

normalizeFrontendMsgUrl :: FrontendMsg -> Maybe FrontendMsg
normalizeFrontendMsgUrl msg = case normalizeTextURI $ msg^.url.uRL of
  Right url' -> Just $ (url.uRL.~url') msg
  Left _ -> Nothing

escapeTitle :: FrontendMsg -> FrontendMsg
escapeTitle = over (title.tITLE) $ TL.toStrict . BL.renderMarkup . BL.text

parseFrontendMsg :: B.ByteString -> Maybe FrontendMsg
parseFrontendMsg = A.decodeStrict

processFrontendMsg :: B.ByteString -> Maybe (FrontendMsg, FrontendReply)
processFrontendMsg msg = case parseFrontendMsg msg of
    Just m -> case normalizeFrontendMsgUrl m of
      Just m' -> Just (escapeTitle m', replyCanonicalUrl  (m^.url) (m'^.url))
      Nothing -> Nothing
    Nothing -> Nothing
