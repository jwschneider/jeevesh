{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RTMInterface where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import qualified Text.URI as URI

newtype RTMResponse a = RTMResponse { rsp :: a } deriving (Generic, Show)
data REcho = EchoResponse { stat :: Text, api_key :: Text, name :: Text, format :: Text, method :: Text} deriving (Generic, Show)
data ErrorInfo = ErrorInfo { code :: Text, msg :: Text } deriving (Generic, Show)
data RError = RError { stat :: Text, err :: ErrorInfo} deriving (Generic, Show)
type Response a = EitherT RError IO a

instance FromJSON RError
instance FromJSON REcho
instance FromJSON ErrorInfo
instance (FromJSON a) => FromJSON (RTMResponse a)


testRTMEndpoint :: IO ()
testRTMEndpoint = runReq defaultHttpConfig $ do
    v <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse $
            "method" =: ("rtm.test.echo" :: String) <>
            "api_key" =: ("60f3e4cadaa2a9b4f3d89bab1ddf3e60" :: String)  <>
            "name" =: ("test" :: String) <>
            "format" =: ("json" :: String)
    liftIO $ print (responseBody v :: Object)

-- rtmEcho name value = runReq defaultHttpConfig $ do
--     json <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse $
--             "method" =: ("rtm.test.echo" :: Text) <>
--             "api_key" =: ("60f3e4cadaa2a9b4f3d89bab1ddf3e60" :: Text)  <>
--             name =: value <>
--             "format" =: ("json" :: Text)
--     let body = responseBody json :: Value
--     liftIO $ print (parseResponse body :: EitherT RError Result REcho)

parseEcho :: Value -> Result (Either RError REcho)
parseEcho val =
    let try = rsp <$> (fromJSON val :: Result (RTMResponse REcho)) in
        case try of Error _ -> Left . rsp <$> (fromJSON val :: Result (RTMResponse RError))
                    _ -> Right <$> try

parseResponse :: forall a. FromJSON a => Value -> Either RError a
parseResponse val =  failWithRError $ Right . rsp <$> (fromJSON val :: Result (RTMResponse a)) <|>
                     Left . rsp <$> (fromJSON val :: Result (RTMResponse RError))

failWithRError :: Result (Either RError a) -> Either RError a
failWithRError (Error s) = Left $ RError "fail" $ ErrorInfo "0" ("parse error: " `append` pack s)
failWithRError (Success val) = val


rtmEcho' :: Text -> IO (Either RError REcho)
rtmEcho' name = runReq defaultHttpConfig $ do
    json <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse $
            "method" =: ("rtm.test.echo" :: Text) <>
            "api_key" =: ("60f3e4cadaa2a9b4f3d89bab1ddf3e60" :: Text)  <>
            "name" =: name <>
            "format" =: ("json" :: Text)
    let body = responseBody json :: Value
    return (parseResponse body :: Either RError REcho)

rtmEcho :: Text -> Response REcho
rtmEcho name = EitherT $ rtmEcho' name

reduceEcho :: Text -> IO Text
reduceEcho echoName = do
    ret <- rtmEcho' echoName
    case ret of
        Left error -> return $ (msg . err) error
        Right resp -> return $ name resp
