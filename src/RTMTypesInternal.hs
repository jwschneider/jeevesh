{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module RTMTypesInternal where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Aeson
import Data.List (foldl')
import Data.Char (toLower)

-- data User = User {id :: String, username :: String, fullname :: String} deriving (Show, Eq)
-- instance FromJSON User where
--     parseJSON = withObject "User" $ \v -> User
--         <$> v .: "id"
--         <*> v .: "username"
--         <*> v .: "fullname"

defBang = Bang NoSourceUnpackedness NoSourceStrictness

strT = ConT ''String

mkRecord :: Name -> [(String, Type)] -> Q Dec
mkRecord rtmTypeName fields = do
    return $ DataD [] rtmTypeName [] Nothing [constr] [derivs]
    where
        constr = RecC rtmTypeName (map mkRow fields)
        derivs = DerivClause Nothing [ConT ''Show, ConT ''Eq]
        mkRow (str, typ) = (mkName ((map toLower . nameBase) rtmTypeName ++ "_" ++ str), defBang, typ)

mkFromJsonInst :: Name -> [String] -> Q Dec
mkFromJsonInst rtmTypeName fields = do
    v <- newName "v"
    let parserExpr = foldl' (\exp str -> [| $exp <*> $(varE v) .: str |]) 
            [| fmap $(conE rtmTypeName) ($(varE v) .: $(litE (stringL (head fields))))|] 
            (tail fields)
    parseJsonExpr <- [| withObject $recNameStr $ $(lamE [varP v] parserExpr) |]
    let parseJsonDecl = FunD 'parseJSON [Clause [] (NormalB parseJsonExpr) []]
    return $ InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT rtmTypeName)) [parseJsonDecl]
    where
        recNameStr = litE (stringL (nameBase rtmTypeName))


genRtmDataType :: Name -> [(String, Type)] -> Q [Dec]
genRtmDataType rtmTypeName fields = do
    record <- mkRecord rtmTypeName fields
    inst <- mkFromJsonInst rtmTypeName (map fst fields)
    return [record, inst]