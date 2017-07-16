module ArbitraryInstances (
    FrontendMsgTest(..)
) where

import BrainData
import Data.List           (intercalate)
import Control.Applicative (pure, liftA2)
import Test.QuickCheck
import Data.Word (Word32)
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.ByteString     as B
import qualified Data.Set            as S
import qualified Data.UUID           as U
import qualified Data.ByteString.Char8 as BChar

generateHttp :: Gen String
generateHttp = elements ["http://", "https://"]

generateWord :: Gen String
generateWord = listOf1 $ choose ('a', 'z')

(<++>) :: Applicative f => f [a] -> f [a] ->  f [a]
xs <++> ys = liftA2 (++) xs ys

(++>) :: Applicative f => [a] -> f [a] ->  f [a]
xs ++> ys = liftA2 (++) (pure xs) ys

(<++) :: Applicative f => f [a] -> [a] ->  f [a]
xs <++ ys = liftA2 (++) xs (pure ys)

dotWord :: Gen String
dotWord = "." ++> generateWord

slashWord :: Gen String
slashWord = "/" ++> generateWord

eqWord :: Gen String
eqWord = generateWord <++ "=" <++> generateWord

listWord :: Gen String -> Gen String
listWord = fmap concat . listOf1

generateUrl :: Gen String
generateUrl = generateHttp <++> generateWord <++> listWord dotWord <++> listWord slashWord

queryAndHash :: Gen String
queryAndHash = "?" ++> eqWord <++ "#" <++> generateWord

data FrontendMsgTest = FrontendMsgTest {
    fmtMsg :: BChar.ByteString
  , fmtUrl :: URL
  , fmtTitle :: Title
} deriving (Show, Eq, Ord)

toFrontendMsgBS :: String -> String -> BChar.ByteString
toFrontendMsgBS url' title' = BChar.pack $ "{ \"url\": \"" ++ url' ++ "\", \"title\":\"" ++ title' ++ "\"}"

instance Arbitrary FrontendMsgTest where
  arbitrary = do
    url' <- generateUrl
    qandh <- queryAndHash
    title' <- generateWord
    let msg = toFrontendMsgBS (url' ++ qandh) title'
    return $ FrontendMsgTest msg (URL $ BChar.pack url') (Title $ T.pack title')
instance Arbitrary Name where
  arbitrary = do
    text <- listOf1 arbitrary
    return $ Name $ B.pack text

instance Arbitrary Title where
  arbitrary = do
    text <- listOf1 arbitrary
    return $ Title $ T.pack text

instance Arbitrary UserUUID where
  arbitrary = do
    w1 <- arbitrary :: Gen Word32
    w2 <- arbitrary :: Gen Word32
    w3 <- arbitrary :: Gen Word32
    w4 <- arbitrary :: Gen Word32
    return $ UserUUID $ U.fromWords w1 w2 w3 w4

instance Arbitrary State where
  arbitrary = do
    let namesInUse = S.empty
    let users = M.empty
    let placeEvents = M.empty
    let places = M.empty
    return $ State namesInUse users placeEvents places

