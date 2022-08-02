module RTMInterface where

import RTMTypes
import RTMApi

import Control.Monad.Trans.Either
import Data.List

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

callApiWithUserAuth :: (String -> String -> String -> Response a) -> String -> String -> Response Auth -> Response a
callApiWithUserAuth method key secret rAuth =
    rAuth >>= checkToken . auth_token >>= method key secret . auth_token
    where
        checkToken tk = rtmCheckToken key tk secret

-- getUserAuthAndCallApi :: (String -> Response a) -> Response a
-- getUserAuthAndCallApi method = EitherT $ do
--     (key, secret) <- getApiKeyAndSharedSecret
--     runEitherT $ callApiWithUserAuth key method (getUserToken key secret) secret

getListId :: String -> String -> String -> Response Auth -> Response String
getListId key listName secret authToken = do
    lists <- lists_list <$> callApiWithUserAuth rtmGetLists key secret authToken
    case find (\list -> list_name list == listName) lists of
        Nothing -> left $ ErrorInfo "0" ("Unable to find list: " ++ listName)
        (Just list) -> right $ list_id list


