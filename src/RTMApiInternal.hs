{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RTMApiInternal where

import RTMTypes

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

import Control.Monad.IO.Class

failWithRError :: Result (Either RError a) -> Either RError a
failWithRError (Error s) = Left $ RError "fail" $ ErrorInfo "0" ("parse error: " ++ s)
failWithRError (Success val) = val

parseResponse :: forall a. FromJSON a => Value -> Either RError a
parseResponse val =  failWithRError $ Right . rsp <$> (fromJSON val :: Result (RTMResponse a)) <|>
                     Left . rsp <$> (fromJSON val :: Result (RTMResponse RError))



generateSignature :: [(String, String)] -> String -> String -- don't commit until api key and shared secret are removed
generateSignature params secret =
    (md5s . Str) $ 
        Data.List.foldl' (\str pair -> str ++ uncurry (++) pair) secret (sortOn fst params)

signRequest :: [(String, String)] -> Maybe String -> [(String, String)]
signRequest params Nothing = params
signRequest params (Just secret) = 
    let signature = generateSignature params secret in
        params ++ [("api_sig", signature)]

-- rtmGetJ :: [(String, String)] -> IO Value
-- rtmGetJ params = do
--     (key, secret) <- getApiKeyAndSharedSecret
--     let signedParams = signRequest (("api_key", key) : ("format", "json") : params) secret
--     runReq defaultHttpConfig $ do
--         json <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse $
--             Data.List.foldl' (\params pair -> params <> ((T.pack . fst) pair =: snd pair)) mempty signedParams
--         return (responseBody json :: Value)

rtmGetJ :: [(String, String)] -> IO Value
rtmGetJ signedParams = 
    runReq defaultHttpConfig $ do
        json <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse $
            Data.List.foldl' (\params pair -> params <> ((T.pack . fst) pair =: snd pair)) mempty signedParams
        return (responseBody json :: Value)

rtmPostJ :: [(String, String)] -> IO Value
rtmPostJ signedParams = 
    runReq defaultHttpConfig $ do
            json <- req POST (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse $
                Data.List.foldl' (\params pair -> params <> ((T.pack . fst) pair =: snd pair)) mempty signedParams
            return (responseBody json :: Value)


defBang = Bang NoSourceUnpackedness NoSourceStrictness

mkResponseRecord :: Name -> Name -> Bool -> Q Dec
mkResponseRecord recName payloadType includeTransaction = do 
    return $ DataD [] recName [] Nothing [constr] [derivs]
    where
        transaction = [(defBang, ConT ''String) | includeTransaction]
        constr = NormalC recName $ [(defBang, ConT ''String), (defBang, ConT payloadType)] ++ transaction
        derivs = DerivClause Nothing [ConT ''Show, ConT ''Eq]

mkInstFromJson :: Name -> Name -> Bool -> Q Dec
mkInstFromJson recName payloadName includeTransaction = do
    v <- newName "v"
    let parserExpr = [| $(conE recName) <$> $(varE v) .: "stat" <*> $(varE v) .: $payloadNameStr |]
    let parserExprWithTransaction = [| $parserExpr <*> $(varE v) .: "transaction"|]
    parseJsonExpr <- [| withObject $recNameStr $ $(lamE [varP v] (if includeTransaction then parserExprWithTransaction else parserExpr))|]
    let parseJsonDecl = FunD 'parseJSON [Clause [] (NormalB parseJsonExpr) []]
    return $ InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT recName)) [parseJsonDecl]
    where
        recNameStr = litE $ stringL (nameBase recName)
        payloadNameStr = litE $ stringL (nameBase payloadName)

numAuthParams :: AuthLevel -> Int
numAuthParams AuthRequired = 2
numAuthParams SignatureRequired = 1
numAuthParams NoAuthRequired = 0

-- rtmFoo :: String -> ... -> Response RFoo
mkSignature :: Name -> Int -> Int -> Int -> Bool -> Name -> Q Dec
mkSignature methodName numParams numOptionalParams numAuthParams includeTransaction payloadType = do
    return $ SigD methodName typeSignature
    where
        params = replicate numParams (ConT ''String)
        optionalParams = replicate numOptionalParams (AppT (ConT ''Maybe) (ConT ''String))
        authParams = replicate numAuthParams (ConT ''String)
        returnType = if includeTransaction then AppT (AppT (TupleT 2) (ConT ''String)) (ConT payloadType) else ConT payloadType
        typeSignature = foldr (AppT . AppT ArrowT) (AppT (ConT ''Response) returnType) (params ++ optionalParams ++ authParams)



-- [("method", "rtm.foo"), ("param1", p_param1) ... ]
mkRequestParams :: [String] -> [Name] -> Q Exp
mkRequestParams requestParams paramNames = pure $ ListE $
    zipWith mkRequestParam requestParams paramNames
    where mkRequestParam param name = TupE [Just (LitE (StringL param)), Just (VarE name)]

filterMaybes :: [(String, Maybe String)] -> [(String, String)]
filterMaybes [] = []
filterMaybes ((str, Nothing) : xs) = filterMaybes xs
filterMaybes ((str, Just str_) : xs) = (str, str_) : filterMaybes xs

mkFieldAccessFnClause :: Name -> Name -> Name -> Name -> Q [Dec]
mkFieldAccessFnClause fieldAccessName payloadName payloadType recName = 
    pure [fieldAccessSig, fieldAccessFun] where
        fieldAccessSig = SigD fieldAccessName (AppT (AppT ArrowT (ConT recName)) (ConT payloadType))
        fieldAccessFun = FunD fieldAccessName [Clause [] (NormalB (VarE payloadName)) []]

mkAuthParams :: AuthLevel -> [String]
mkAuthParams NoAuthRequired = empty
mkAuthParams SignatureRequired = "shared_secret" : mkAuthParams NoAuthRequired
mkAuthParams AuthRequired = "auth_token" : mkAuthParams SignatureRequired

mkAuthParamNames :: AuthLevel -> [Name]
mkAuthParamNames authLevel = map mkName (mkAuthParams authLevel)


expFromMaybe :: Maybe Name -> Q Exp
expFromMaybe Nothing = conE 'Nothing
expFromMaybe (Just name) = appE (conE 'Just) (varE name)

-- returnLambda :: Name -> Bool -> Q Exp
-- returnLambda recName includeTransaction = 
--     if includeTransaction then
--         [| \resp -> ($(varE transactionAccessName) <$> resp, $(varE payloadAccessName) <$> resp) |]
--     else
--         [| \resp -> bimap err (\($(conE recName) _ payload transaction) -> payload) resp |]
--     where
--         successLambda = \()

lambdaSuccess :: Name -> Bool -> Q Exp
lambdaSuccess recName includeTransaction = do
    trans <- newName "transaction"
    payload <- newName "payload"
    let expr = if includeTransaction then TupE [Just (VarE trans), Just (VarE payload)]
                    else VarE payload
    return $ LamE [ConP recName ([WildP, VarP payload] ++ (if includeTransaction then [VarP trans] else empty))] expr

rtmCallJ :: String -> Q Exp
rtmCallJ "GET" = [| rtmGetJ |]
rtmCallJ "POST" = [| rtmPostJ |]
rtmCallJ _ = fail "Only supported HTTP methods are GET and POST"

-- rtmFoo p_param1 ... = EitherT $ do
--     body <- rtmGetJ [("method", "rtm.foo"), ("param1", param1) ...]
--     return (parseResponse body :: Either RError RFoo)
mkMethod :: Name -> String -> String -> [String] -> [String] -> AuthLevel -> Bool -> Name -> Name -> Name -> Name -> Q Dec
mkMethod methodName method httpMethod requestParams requestOptionalParams authLevel includeTransaction payloadName payloadType transactionName recName = do
    signedParams <- newName "signedParams"
    paramNames <- mapM newName requestParams
    optionalParamNames <- mapM newName requestOptionalParams
    authParamNames <- mapM newName authParams
    let sharedSecret = find (\n -> "shared_secret" == nameBase n) authParamNames
    pat <- mapM varP (paramNames ++ optionalParamNames ++ authParamNames)
    expr <- [| EitherT $ do
                body <- $(rtmCallJ httpMethod) $(varE signedParams)
                let resp = parseResponse body :: Either RError $(conT recName)
                return (bimap err $(lambdaSuccess recName includeTransaction) resp)
            |]
    wherecl<- [d| $(varP signedParams) = 
                        signRequest ([("method", $(litE (stringL method))), ("format", "json")] 
                                        ++ $(mkRequestParams requestParams paramNames)
                                        ++ filterMaybes $(mkRequestParams requestOptionalParams optionalParamNames)
                                        ++ $(mkRequestParams authParams authParamNames))
                                    $(expFromMaybe sharedSecret) |]
    return $ FunD methodName [Clause pat (NormalB expr) wherecl]
    where
        authParams = mkAuthParams authLevel

genRtmMethod :: String -> String -> String -> String -> Name -> [String] -> [String] -> AuthLevel -> Q [Dec]
genRtmMethod rtmName method httpMethod payload payloadType requestParams requestOptionalParams authLevel = do
    payloadName <- newName payload
    transactionName <- newName "transaction"
    record <- mkResponseRecord recName payloadType includeTransaction
    inst <- mkInstFromJson recName payloadName includeTransaction
    signature <- mkSignature methodName (length params) (length requestOptionalParams) (numAuthParams authLevel) includeTransaction payloadType
    method <- mkMethod methodName method httpMethod params requestOptionalParams authLevel includeTransaction payloadName payloadType transactionName recName
    return [record, inst, signature, method]
    where
        params = "api_key" : requestParams
        recName = mkName ("R" ++ rtmName)
        includeTransaction = httpMethod == "POST"
        methodName = mkName ("rtm" ++ rtmName)