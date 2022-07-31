{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module RTMApi where

import Language.Haskell.TH.Syntax
import RTMTypes
import RTMApiInternal
import Data.List (foldl')
import Web.Browser (openBrowser)
import Control.Monad

-- rtmEcho :: String -> Response T.Text
-- rtmEcho p_name = EitherT $ do
--     body <- rtmGetJ [("method", "rtm.test.echo"), ("name", name)]
--     return f_name <$> (parseResponse body :: Either RError REcho)
--     where
--         f_name :: REcho -> T.Text
--         f_name = name
$(genRtmMethod "Echo" "rtm.test.echo" "GET" "name" ''String ["name"] [] NoAuthRequired)
$(genRtmMethod "Frob" "rtm.auth.getFrob" "GET" "frob" ''String [] [] SignatureRequired)
-- $(genRtmMethod "GetToken" "rtm.auth.getToken" "GET" "auth" ''Auth ["frob"] [] SignatureRequired)
-- -- $(genRtmMethod "CheckToken" "rtm.auth.checkToken" "auth" ''Auth ["auth_token"])
-- -- $(genRtmMethod "GetLists" "rtm.lists.getList" "lists" ''Lists ["auth_token"])
-- $(genRtmMethod "TasksGetList" "rtm.tasks.getList" "GET" "list" ''List [] ["list_id", "filter", "last_sync", "callback"] AuthRequired)

userAuth :: String -> String -> String -> IO ()
userAuth frob api_key shared_secret = do
    let endpoint = "https://www.rememberthemilk.com/services/auth/"
    let params = [("api_key", api_key), ("perms", "delete"), ("frob", frob)]
    openBrowser $ endpoint ++ "?" ++ 
        tail (foldl' (\b (k,  v) -> b ++ "&" ++ k ++ "=" ++ v) "" (signRequest params (Just shared_secret)))
    putStrLn "Press any key when complete with authentication"
    void getChar