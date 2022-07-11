{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module RTMTypes where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.Generics (Generic)
import Data.Text
import Data.Aeson


defBang = Bang NoSourceUnpackedness NoSourceStrictness


mkResponseRecord :: String -> String -> Name -> Q [Dec]
mkResponseRecord recName payloadName payloadType = do 
    let decl = DataD [] typeName [] Nothing [constr] [derivs]
    let inst = InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT typeName)) []
    return [decl, inst]
    where
        typeName = mkName $ "R" ++ recName
        constr = RecC typeName [(mkName payloadName, defBang, ConT payloadType),
                                (mkName "stat", defBang, ConT ''Text)]
        derivs = DerivClause Nothing [ConT ''Show, ConT ''Eq, ConT ''Generic]
