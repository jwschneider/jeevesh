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


