{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric     #-}


module RTMTypes where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Aeson
import Data.List
import Control.Monad
import Data.Bifunctor
import Control.Monad.Trans.Either
import Data.Monoid
import Control.Applicative
import Data.Hash.MD5
import Network.HTTP.Req


newtype RTMResponse a = RTMResponse { rsp :: a } deriving (Generic, Show)
data ErrorInfo = ErrorInfo { code :: T.Text, msg :: T.Text } deriving (Generic, Show, Eq)
data RError = RError { stat :: T.Text, err :: ErrorInfo} deriving (Generic, Show, Eq)
type Response a = EitherT RError IO a


instance FromJSON RError
instance FromJSON ErrorInfo
instance (FromJSON a) => FromJSON (RTMResponse a)

failWithRError :: Result (Either RError a) -> Either RError a
failWithRError (Error s) = Left $ RError "fail" $ ErrorInfo "0" ("parse error: " `T.append` T.pack s)
failWithRError (Success val) = val

parseResponse :: forall a. FromJSON a => Value -> Either RError a
parseResponse val =  failWithRError $ Right . rsp <$> (fromJSON val :: Result (RTMResponse a)) <|>
                     Left . rsp <$> (fromJSON val :: Result (RTMResponse RError))

getApiKeyAndSharedSecret :: IO (T.Text, T.Text)
getApiKeyAndSharedSecret = do
    csv <- readFile "rtm-id.secret"
    let (key, secret) = break (==',') csv in
        return (T.pack key, T.pack (drop 1 secret))

generateSignature :: [(T.Text, T.Text)] -> T.Text -> T.Text -- don't commit until api key and shared secret are removed
generateSignature params secret =
    (T.pack . md5s . Str . T.unpack) $ 
        Data.List.foldl' (\str pair -> str `T.append` fst pair `T.append` snd pair) secret (sortOn fst params)

signRequest :: [(T.Text, T.Text)] -> T.Text -> [(T.Text, T.Text)]
signRequest params secret = 
    let signature = generateSignature params secret in
        params ++ [("api_sig", signature)]

rtmGetJ :: [(T.Text, T.Text)] -> IO Value
rtmGetJ params = do
    (key, secret) <- getApiKeyAndSharedSecret
    let signedParams = signRequest (("api_key", key) : ("format", "json") : params) secret
    runReq defaultHttpConfig $ do
        json <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse $
            Data.List.foldl' (\str pair -> str <> uncurry (=:) pair) mempty signedParams
        return (responseBody json :: Value)



defBang = Bang NoSourceUnpackedness NoSourceStrictness

mkResponseRecord :: Name -> Name -> Name -> Q [Dec]
mkResponseRecord recName payloadName payloadType = do 
    let decl = DataD [] recName [] Nothing [constr] [derivs]
    let inst = InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT recName)) []
    return [decl, inst]
    where
        constr = RecC recName [(payloadName, defBang, ConT payloadType),
                                (mkName "stat", defBang, ConT ''T.Text)]
        derivs = DerivClause Nothing [ConT ''Show, ConT ''Eq, ConT ''Generic]

-- rtmFoo :: String -> ... -> Response RFoo
mkSignature :: Name -> Int -> Name -> Q [Dec]
mkSignature methodName numParams returnType = do
    (pure . pure) $ SigD methodName typ
    where
        rptText = replicate numParams (ConT ''String)
        typ = foldl' (\sig t -> AppT (AppT ArrowT t) sig) (AppT (ConT ''Response) (ConT returnType)) rptText


-- [("method", "rtm.foo"), ("param1", p_param1) ... ]
mkRequestParams :: String -> [String] -> [Name] -> Q Exp
mkRequestParams endpoint requestParams paramNames = do
    pure $ ListE (TupE [Just (LitE (StringL "method")), Just (LitE (StringL endpoint))] :
        zipWith (\str nam -> TupE [Just (LitE (StringL str)), Just (VarE nam)]) requestParams paramNames)

-- rtmFoo p_param1 ... = EitherT $ do
--     body <- rtmGetJ [("method", "rtm.foo"), ("param1", param1) ...]
--     return (parseResponse body :: Either RError RFoo)
mkMethod :: Name -> String -> [String] -> Name -> Name -> Q [Dec]
mkMethod methodName endpoint requestParams recName payloadName = do
    expr <- [| EitherT $ do 
                body <- rtmGetJ $ map (join bimap T.pack) $(mkRequestParams endpoint requestParams paramNames)
                return $ $(varE payloadName) <$> (parseResponse body :: Either RError $(conT recName))
            |]
    (pure. pure) $ FunD methodName [Clause pat (NormalB expr) []]
    where
        paramNames = map (mkName . ("p_" ++)) requestParams
        pat = map VarP paramNames

genRtmMethod :: String -> String -> String -> Name -> [String] -> Q [Dec]
genRtmMethod rtmName endpoint payload payloadType requestParams = do
    record <- mkResponseRecord recName payloadName payloadType
    sigature <- mkSignature methodName (length requestParams) payloadType
    method <- mkMethod methodName endpoint requestParams recName payloadName
    return (record ++ sigature ++ method)
    where
        recName = mkName ("R" ++ rtmName)
        payloadName = mkName payload
        methodName = mkName ("rtm" ++ rtmName)

