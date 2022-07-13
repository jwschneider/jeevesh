{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module RTMInterface where

import RTMTypes
import Language.Haskell.TH.Syntax
import qualified Data.Text as T
import Web.Browser (openBrowser)
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


userAuth :: T.Text -> IO ()
userAuth frob = do
    (key, secret) <- getApiKeyAndSharedSecret
    let endpoint = "https://www.rememberthemilk.com/services/auth/"
    let params = [("api_key", key), ("perms", "delete"), ("frob", frob)]
    openBrowser $ endpoint ++ "?" ++ 
        (tail . T.unpack) (foldl' (\b (k,  v) -> b `T.append` "&" `T.append` k `T.append` "=" `T.append` v) T.empty (signRequest params secret))
    putStrLn "Press any key when complete with authentication"
    void getChar


getUserToken :: Response Auth
getUserToken = EitherT $ do
    frobE <- runEitherT rtmFrob
    case frobE of
        Left error -> return (Left $ RError "fail" $ ErrorInfo "1" ("failed to get frob: " `T.append` (msg . err) error))
        Right frob -> do
            userAuth frob
            runEitherT $ rtmGetToken (T.unpack frob)

