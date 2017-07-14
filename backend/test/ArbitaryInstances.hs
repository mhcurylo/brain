module Test.Util.ArbitraryInstances (
    ArbitraryFrontendMsg
  ) where

import Data.List           (intercalate)
import qualified Data.ByteString     as B
import Control.Applicative ((<$>), (<*>), pure)
import Test.QuickCheck

data ArbitraryFrontendMsg = ArbitraryFrontendMsg {
    afmMsg :: B.ByteString
  , afmUrl :: B.ByteString
  , afmTitle :: B.ByteString
}
