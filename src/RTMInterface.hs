module RTMInterface where

import RTMTypes
import RTMApi

import Control.Monad.Trans.Either

getUserToken :: Response Auth
getUserToken = rtmFrob >>= getAuth
    where
        getAuth frob = EitherT $ do
            userAuth frob
            runEitherT $ rtmGetToken frob

callApiWithUserAuth :: (String -> Response a) -> Response Auth -> Response a
callApiWithUserAuth method rAuth =
    rAuth >>= rtmCheckToken . token >>= method . token

getUserAuthAndCallApi :: (String -> Response a) -> Response a
getUserAuthAndCallApi method = 
        callApiWithUserAuth method getUserToken

-- getListId :: String -> String -> Response String
-- getListId listName authToken =
--     (callApiWithUserAuth rtmGetLists authToken)


-- TODO update API to support methods which have multiple optional arguments and which may return more than one thing