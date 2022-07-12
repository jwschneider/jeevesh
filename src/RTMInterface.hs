{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module RTMInterface where

import RTMTypes
import Language.Haskell.TH.Syntax

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromJust)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Sort
import Data.List
import Data.Word (Word8)
import Data.Bifunctor (bimap)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString as B
import qualified Crypto.Hash.MD5 as MD5
import qualified Text.URI as URI
import Web.Internal.HttpApiData
import Data.Hash.MD5

newtype RTMResponse a = RTMResponse { rsp :: a } deriving (Generic, Show)
data ErrorInfo = ErrorInfo { code :: T.Text, msg :: T.Text } deriving (Generic, Show, Eq)
data RError = RError { stat :: T.Text, err :: ErrorInfo} deriving (Generic, Show, Eq)
type Response a = EitherT RError IO a


instance FromJSON RError
instance FromJSON ErrorInfo
instance (FromJSON a) => FromJSON (RTMResponse a)

$(mkResponseRecord (mkName "REcho") (mkName "name") ''T.Text)
$(mkResponseRecord (mkName "RFrob") (mkName "frob") ''T.Text)

testRTMEndpoint :: IO ()
testRTMEndpoint = runReq defaultHttpConfig $ do
    v <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse $
            "method" =: ("rtm.test.echo" :: String) <>
            "api_key" =: ("60f3e4cadaa2a9b4f3d89bab1ddf3e60" :: String)  <>
            "name" =: ("test" :: String) <>
            "format" =: ("json" :: String)
    liftIO $ print (responseBody v :: Object)


getApiKeyAndSharedSecret :: IO (T.Text, T.Text)
getApiKeyAndSharedSecret = do
    csv <- readFile "rtm-id.secret"
    let (key, secret) = break (==',') csv in
        return (T.pack key, T.pack (drop 1 secret))


parseEcho :: Value -> Result (Either RError REcho)
parseEcho val =
    let try = rsp <$> (fromJSON val :: Result (RTMResponse REcho)) in
        case try of Error _ -> Left . rsp <$> (fromJSON val :: Result (RTMResponse RError))
                    _ -> Right <$> try

parseResponse :: forall a. FromJSON a => Value -> Either RError a
parseResponse val =  failWithRError $ Right . rsp <$> (fromJSON val :: Result (RTMResponse a)) <|>
                     Left . rsp <$> (fromJSON val :: Result (RTMResponse RError))

failWithRError :: Result (Either RError a) -> Either RError a
failWithRError (Error s) = Left $ RError "fail" $ ErrorInfo "0" ("parse error: " `T.append` T.pack s)
failWithRError (Success val) = val

rtmGetJ :: [(T.Text, T.Text)] -> IO Value
rtmGetJ params = do
    (key, secret) <- getApiKeyAndSharedSecret
    let signedParams = signRequest (("api_key", key) : ("format", "json") : params) secret
    runReq defaultHttpConfig $ do
        json <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse $
            Data.List.foldl' (\str pair -> str <> uncurry (=:) pair) mempty signedParams
        return (responseBody json :: Value)

rtmEcho :: T.Text -> Response REcho
rtmEcho name = EitherT $ do
    body <- rtmGetJ [("method", "rtm.test.echo"), ("name", name)]
    return (parseResponse body :: Either RError REcho)

rtmFrob :: Response RFrob
rtmFrob = EitherT $ do
    body <- rtmGetJ [("method", "rtm.auth.getFrob")]
    return (parseResponse body :: Either RError RFrob)

generateSignature :: [(T.Text, T.Text)] -> T.Text -> T.Text -- don't commit until api key and shared secret are removed
generateSignature params secret =
    (T.pack . md5s . Str . T.unpack) $ 
        Data.List.foldl' (\str pair -> str `T.append` fst pair `T.append` snd pair) secret (sortOn fst params)

signRequest :: [(T.Text, T.Text)] -> T.Text -> [(T.Text, T.Text)]
signRequest params secret = 
    let signature = generateSignature params secret in
        params ++ [("api_sig", signature)]