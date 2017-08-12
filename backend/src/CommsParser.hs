module CommsParser where

import BrainData
import BrainMsg
import Control.Lens ((.~), (^.))
import qualified Data.ByteString     as B
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified URI.ByteString      as URI
import qualified Data.Aeson          as A


emptyQuery :: URI.Query
emptyQuery = URI.Query []

canonicalForm :: URI.URIRef URI.Absolute -> URI.URIRef URI.Absolute
canonicalForm = (URI.queryL.~emptyQuery) . (URI.fragmentL.~Nothing)

normalizeURI :: URI.URIRef URI.Absolute -> T.Text
normalizeURI = T.decodeUtf8 . URI.normalizeURIRef' URI.aggressiveNormalization . canonicalForm

normalizeTextURI :: T.Text -> Either URI.URIParseError T.Text
normalizeTextURI = fmap normalizeURI . URI.parseURI URI.strictURIParserOptions . T.encodeUtf8

normalizeFrontendMsg :: FrontendMsg -> Maybe FrontendMsg
normalizeFrontendMsg msg = case normalizeTextURI $ msg^.url.uRL of
  Right url' -> Just $ (url.uRL.~url') msg
  Left _ -> Nothing

parseFrontendMsg :: B.ByteString -> Maybe FrontendMsg
parseFrontendMsg = A.decodeStrict

processFrontendMsg :: B.ByteString -> Maybe (FrontendMsg, FrontendReply)
processFrontendMsg msg = case parseFrontendMsg msg of
    Just m -> case normalizeFrontendMsg m of
      Just m' -> Just (m', replyCanonicalUrl  (m^.url) (m'^.url))
      Nothing -> Nothing
    Nothing -> Nothing
