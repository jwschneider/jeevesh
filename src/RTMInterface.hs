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
$(genRtmMethod "Echo" "rtm.test.echo" "name" ''T.Text ["name"])
$(genRtmMethod "Frob" "rtm.auth.getFrob" "frob" ''T.Text [])
$(genRtmMethod "GetToken" "rtm.auth.getToken" "auth" ''Auth ["frob"])
$(genRtmMethod "CheckToken" "rtm.auth.checkToken" "auth" ''Auth ["auth_token"])
$(genRtmMethod "GetList" "rtm.lists.getList" "lists" ''Lists ["auth_token"])

userAuth :: T.Text -> IO ()
userAuth frob = do
    (key, secret) <- getApiKeyAndSharedSecret
    let endpoint = "https://www.rememberthemilk.com/services/auth/"
    let params = [("api_key", key), ("perms", "delete"), ("frob", frob)]
    openBrowser $ endpoint ++ "?" ++ 
        (tail . T.unpack) (foldl' (\b (k,  v) -> b `T.append` "&" `T.append` k `T.append` "=" `T.append` v) T.empty (signRequest params secret))
    putStrLn "Press any key when complete with authentication"
    void getChar

-- mapOnRight f rspA = rspA >>= f

getUserToken :: Response Auth
getUserToken = rtmFrob >>= getAuth
    where
        getAuth frob = EitherT $ do
            userAuth frob
            runEitherT $ rtmGetToken (T.unpack frob)
-- getUserToken = bracketEitherT rtmFrob (EitherT . fmap Right . userAuth) (rtmGetToken . T.unpack)

-- getValidUserToken :: Either RError Auth -> Response Auth
-- getValidUserToken Left _ = getUserToken
-- getValidUserToken Right auth1 = do
--     auth2 <- runEitherT $ rtmCheckToken (token auth1)
--     getValidUserToken auth2


callApiWithUserAuth :: (String -> Response a) -> Response Auth -> Response a
callApiWithUserAuth method rAuth =
    let rAuth' = secondEitherT (T.unpack . token) rAuth in
        rAuth' >>= method

getUserAuthAndCallApi :: (String -> Response a) -> Response a
getUserAuthAndCallApi method = 
    let rAuth = getUserToken in
        callApiWithUserAuth method rAuth

-- callApiWithUserAuth method (Just auth) = EitherT $ do
--     vAuth <- runEitherT $ rtmCheckToken (T.unpack (token auth))
--     case vAuth of 
--         Left _ -> runEitherT $ callApiWithUserAuth method Nothing
--         Right auth' -> runEitherT $ method (T.unpack (token auth))
-- callApiWithUserAuth method Nothing =
--     let rsp = getUserToken in
--         mapOnRight (callApiWithUserAuth method) (Just rsp)


