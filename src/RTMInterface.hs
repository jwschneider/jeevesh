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
import Data.Monoid ((<>))
import Data.Text
import Data.Sort
import Data.List
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import qualified Text.URI as URI

newtype RTMResponse a = RTMResponse { rsp :: a } deriving (Generic, Show)
-- data REcho = EchoResponse { stat :: Text, api_key :: Text, name :: Text, format :: Text, method :: Text} deriving (Generic, Show, Eq)
data ErrorInfo = ErrorInfo { code :: Text, msg :: Text } deriving (Generic, Show, Eq)
data RError = RError { stat :: Text, err :: ErrorInfo} deriving (Generic, Show, Eq)
type Response a = EitherT RError IO a


instance FromJSON RError
instance FromJSON ErrorInfo
instance (FromJSON a) => FromJSON (RTMResponse a)

$(mkResponseRecord "Echo" "name" ''Text)
$(mkResponseRecord "Frob" "frob" ''Text)

testRTMEndpoint :: IO ()
testRTMEndpoint = runReq defaultHttpConfig $ do
    v <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse $
            "method" =: ("rtm.test.echo" :: String) <>
            "api_key" =: ("60f3e4cadaa2a9b4f3d89bab1ddf3e60" :: String)  <>
            "name" =: ("test" :: String) <>
            "format" =: ("json" :: String)
    liftIO $ print (responseBody v :: Object)


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


rtmGetJ :: [(Text, Text)] -> IO Value
rtmGetJ params = runReq defaultHttpConfig $ do
    json <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse $
        Data.List.foldl' (\str pair -> str <> uncurry (=:) pair) mempty (("format", "json") : params)
    return (responseBody json :: Value)

rtmEcho :: Text -> Response REcho
rtmEcho name = EitherT $ do
    body <- rtmGetJ [("method", "rtm.test.echo"), ("api_key", "60f3e4cadaa2a9b4f3d89bab1ddf3e60"), ("name", name)]
    return (parseResponse body :: Either RError REcho)

-- rtmFrob :: Response RFrob

signRequest :: [(Text, Text)] -> Text
signRequest params = 
    Data.List.foldl' (\str pair -> str `append` fst pair `append` snd pair) Data.Text.empty (sortOn fst params)