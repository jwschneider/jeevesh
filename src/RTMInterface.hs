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
import Data.List (foldl')

-- rtmEcho :: T.Text -> Response REcho
-- rtmEcho name = EitherT $ do
--     body <- rtmGetJ [("method", "rtm.test.echo"), ("name", name)]
--     return (parseResponse body :: Either RError REcho)
$(genRtmMethod "Echo" "rtm.test.echo" "name" ''T.Text ["name"])
$(genRtmMethod "Frob" "rtm.auth.getFrob" "frob" ''T.Text [])

userAuth :: IO ()
userAuth = do
    (key, secret) <- getApiKeyAndSharedSecret
    frobE <- runEitherT rtmFrob
    case frobE of 
        Left error -> print $ err error
        Right rfrob ->  do 
            openBrowser $ endpoint ++ "?" ++ 
                (tail . T.unpack) (foldl' (\b (k,  v) -> b `T.append` "&" `T.append` k `T.append` "=" `T.append` v) T.empty (signRequest params secret))
            putStrLn "Press any key to continue"
            void getChar
            where
                endpoint = "https://www.rememberthemilk.com/services/auth/"
                params = [("api_key", key), ("perms", "delete"), ("frob", frob rfrob)]