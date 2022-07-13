{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import qualified Data.ByteString as B
import qualified Crypto.Hash.MD5 as MD5
import qualified Text.URI as URI
import Web.Internal.HttpApiData


$(genRtmMethod "Frob" "rtm.auth.getFrob" "frob" ''T.Text [])

-- $(mkResponseRecord (mkName "REcho") (mkName "name") ''T.Text)
$(genRtmMethod "Echo" "rtm.test.echo" "name" ''T.Text ["name"])


-- $(mkResponseRecord (mkName "RFrob") (mkName "frob") ''T.Text)

-- testRTMEndpoint :: IO ()
-- testRTMEndpoint = runReq defaultHttpConfig $ do
--     v <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse $
--             "method" =: ("rtm.test.echo" :: String) <>
--             "api_key" =: ("60f3e4cadaa2a9b4f3d89bab1ddf3e60" :: String)  <>
--             "name" =: ("test" :: String) <>
--             "format" =: ("json" :: String)
--     liftIO $ print (responseBody v :: Object)





-- parseEcho :: Value -> Result (Either RError REcho)
-- parseEcho val =
--     let try = rsp <$> (fromJSON val :: Result (RTMResponse REcho)) in
--         case try of Error _ -> Left . rsp <$> (fromJSON val :: Result (RTMResponse RError))
--                     _ -> Right <$> try







-- rtmEcho :: T.Text -> Response REcho
-- rtmEcho name = EitherT $ do
--     body <- rtmGetJ [("method", "rtm.test.echo"), ("name", name)]
--     return (parseResponse body :: Either RError REcho)

-- rtmFrob :: Response RFrob
-- rtmFrob = EitherT $ do
--     body <- rtmGetJ [("method", "rtm.auth.getFrob")]
--     return (parseResponse body :: Either RError RFrob)

