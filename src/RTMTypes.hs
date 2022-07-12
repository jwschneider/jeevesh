{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module RTMTypes where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Aeson
import Data.List


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


mkSignature :: Name -> Int -> Name -> Q [Dec]
mkSignature methodName numParams returnType = do
    (pure . pure) $ SigD methodName typ
    where
        rptText = replicate numParams (ConT ''T.Text)
        typ = foldl' (\sig t -> AppT (AppT t ArrowT) sig) (ConT returnType) rptText

mkMethod :: Name -> String -> [String] -> Name -> Q [Dec]
mkMethod methodName endpoint requestParams recName = do
    expr <- [| 1 |]
    (pure. pure) $ FunD methodName [Clause pat (NormalB expr) []]
    where
        paramNames = map mkName requestParams
        pat = map VarP paramNames



genRtmMethod :: String -> String -> String -> Name -> [T.Text] -> Q [Dec]
genRtmMethod name endpoint payload payloadType requestParams = do
    recordAndInst <- mkResponseRecord recName payloadName payloadType
    sigature <- [d| |]
    method <- [d| |]
    return (recordAndInst ++ sigature ++ method)
    where
        recName = mkName ("R" ++ name)
        payloadName = mkName payload
        methodName = mkName ("rtm" ++ name)

