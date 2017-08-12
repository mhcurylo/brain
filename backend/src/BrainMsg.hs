{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module BrainMsg where

import qualified Data.Aeson          as A
import qualified Data.Aeson.TH       as A
import qualified Data.Time.Clock     as TC
import Control.Lens hiding (at)
import BrainData

data FrontendMsg = FrontendMsg {
    _url :: URL
  , _title :: Title
} deriving (Show, Eq, Ord)
makeLenses ''FrontendMsg
A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1} ''FrontendMsg

data PAGE_EVENT_ACTION = PAGE_EVENT_ACTION deriving (Show, Eq, Ord);
A.deriveJSON A.defaultOptions ''PAGE_EVENT_ACTION

data CANONICAL_URL_ACTION = CANONICAL_URL_ACTION deriving (Show, Eq, Ord);
A.deriveJSON A.defaultOptions ''CANONICAL_URL_ACTION

data PageEventPayload = PageEventPayload {
    _at :: FrontendMsg
  , _from :: Maybe FrontendMsg
  , _req :: FrontendMsg
  , _when :: TC.UTCTime
  , _who :: Name
} deriving (Show, Eq, Ord);
makeLenses ''PageEventPayload
A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1, A.sumEncoding = A.UntaggedValue} ''PageEventPayload

data CanonicalUrlPayload = CanonicalUrlPayload {
    _original :: URL
  , _canonical :: URL
} deriving (Show, Eq, Ord);
makeLenses ''CanonicalUrlPayload
A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1, A.sumEncoding = A.UntaggedValue} ''CanonicalUrlPayload

data FrontendReply = PageEventReply {
    _PEkind :: PAGE_EVENT_ACTION
  , _PEpayload :: PageEventPayload
} | CanonicalUrlReply {
    _CAkind :: CANONICAL_URL_ACTION
  , _CApayload :: CanonicalUrlPayload
}  deriving (Show, Eq, Ord)

replyPageEvent :: FrontendMsg -> Maybe FrontendMsg -> FrontendMsg -> TC.UTCTime -> Name -> FrontendReply
replyPageEvent a f r we wo = PageEventReply PAGE_EVENT_ACTION $ PageEventPayload a f r we wo

replyCanonicalUrl :: URL -> URL -> FrontendReply
replyCanonicalUrl org can = CanonicalUrlReply CANONICAL_URL_ACTION $ CanonicalUrlPayload org can

makeLenses ''FrontendReply
A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 3, A.sumEncoding = A.UntaggedValue} ''FrontendReply

placeFrontendMsg :: Place -> FrontendMsg
placeFrontendMsg (Place t u _ _) = FrontendMsg u t

type FrontendReplies = [(ConnectedUsers, FrontendReply)]
