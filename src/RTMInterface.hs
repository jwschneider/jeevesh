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

-- rtmEcho :: String -> Response T.Text
-- rtmEcho p_name = EitherT $ do
--     body <- rtmGetJ [("method", "rtm.test.echo"), ("name", name)]
--     return f_name <$> (parseResponse body :: Either RError REcho)
--     where
--         f_name :: REcho -> T.Text
--         f_name = name
$(genRtmMethod "Echo" "rtm.test.echo" "name" ''String ["name"])
$(genRtmMethod "Frob" "rtm.auth.getFrob" "frob" ''String [])
$(genRtmMethod "GetToken" "rtm.auth.getToken" "auth" ''Auth ["frob"])
$(genRtmMethod "CheckToken" "rtm.auth.checkToken" "auth" ''Auth ["auth_token"])
$(genRtmMethod "GetList" "rtm.lists.getList" "lists" ''Lists ["auth_token"])

userAuth :: String -> IO ()
userAuth frob = do
    (key, secret) <- getApiKeyAndSharedSecret
    let endpoint = "https://www.rememberthemilk.com/services/auth/"
    let params = [("api_key", key), ("perms", "delete"), ("frob", frob)]
    openBrowser $ endpoint ++ "?" ++ 
        tail (foldl' (\b (k,  v) -> b ++ "&" ++ k ++ "=" ++ v) "" (signRequest params secret))
    putStrLn "Press any key when complete with authentication"
    void getChar

-- mapOnRight f rspA = rspA >>= f

getUserToken :: Response Auth
getUserToken = rtmFrob >>= getAuth
    where
        getAuth frob = EitherT $ do
            userAuth frob
            runEitherT $ rtmGetToken frob
-- getUserToken = bracketEitherT rtmFrob (EitherT . fmap Right . userAuth) (rtmGetToken . T.unpack)

callApiWithUserAuth :: (String -> Response a) -> Response Auth -> Response a
callApiWithUserAuth method rAuth =
    rAuth >>= rtmCheckToken . token >>= method . token



getUserAuthAndCallApi :: (String -> Response a) -> Response a
getUserAuthAndCallApi method = 
        callApiWithUserAuth method getUserToken


