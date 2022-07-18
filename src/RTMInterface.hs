{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module RTMInterface where

import RTMTypes
import Language.Haskell.TH.Syntax
import qualified Data.Text as T
import Web.Browser (openBrowser)
import Data.Either
import Control.Monad.Trans.Either
import Control.Monad
import Control.Monad.IO.Class
import Data.List (foldl')

-- rtmEcho :: T.Text -> Response REcho
-- rtmEcho p_name = EitherT $ do
--     body <- rtmGetJ [("method", "rtm.test.echo"), ("name", name)]
--     return (parseResponse body :: Either RError REcho)
$(genRtmMethod "Echo" "rtm.test.echo" "name" ''T.Text ["name"]) -- do not make param names the same as payload names
$(genRtmMethod "Frob" "rtm.auth.getFrob" "frob" ''T.Text [])
$(genRtmMethod "GetToken" "rtm.auth.getToken" "auth" ''Auth ["frob"])
$(genRtmMethod "CheckToken" "rtm.auth.checkToken" "auth" ''Auth ["auth_token"])


userAuth :: T.Text -> IO ()
userAuth frob = do
    (key, secret) <- getApiKeyAndSharedSecret
    let endpoint = "https://www.rememberthemilk.com/services/auth/"
    let params = [("api_key", key), ("perms", "delete"), ("frob", frob)]
    openBrowser $ endpoint ++ "?" ++ 
        (tail . T.unpack) (foldl' (\b (k,  v) -> b `T.append` "&" `T.append` k `T.append` "=" `T.append` v) T.empty (signRequest params secret))
    putStrLn "Press any key when complete with authentication"
    void getChar


mapOnRight :: Monad m => (a -> EitherT x m b) -> EitherT x m a -> EitherT x m b
mapOnRight f eithrt = EitherT $ do
    eithr <- runEitherT eithrt
    case eithr of
        Right a -> runEitherT $ f a
        Left x -> return (Left x)

getUserToken :: Response Auth
getUserToken = mapOnRight getAuth rtmFrob
    where
        getAuth frob = EitherT $ do
            userAuth frob
            runEitherT $ rtmGetToken (T.unpack frob)

-- getValidUserToken :: Either RError Auth -> Response Auth
-- getValidUserToken Left _ = getUserToken
-- getValidUserToken Right auth1 = do
--     auth2 <- runEitherT $ rtmCheckToken (token auth1)
--     getValidUserToken auth2


