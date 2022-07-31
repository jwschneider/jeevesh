module RTMInterface where

import RTMTypes
import RTMApi

import Control.Monad.Trans.Either

getApiKeyAndSharedSecret :: IO (String, String)
getApiKeyAndSharedSecret = do
    csv <- readFile "rtm-id.secret"
    let (key, secret) = break (==',') csv in
        return (key, drop 1 secret)

getUserToken :: String -> String -> Response Auth
getUserToken key secret = rtmFrob key secret >>= getAuth key secret
    where
        getAuth key secret frob = EitherT $ do
            userAuth key frob secret
            runEitherT $ rtmGetToken key frob secret

-- callApiWithUserAuth :: (String -> Response a) -> Response Auth -> Response a
-- callApiWithUserAuth method rAuth =
--     rAuth >>= rtmCheckToken . token >>= method . token

-- getUserAuthAndCallApi :: (String -> Response a) -> Response a
-- getUserAuthAndCallApi method = 
--         callApiWithUserAuth method getUserToken

-- getListId :: String -> String -> Response String
-- getListId listName authToken =
--     (callApiWithUserAuth rtmGetLists authToken)


-- TODO update API to support methods which have multiple optional arguments and which may return more than one thing