{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module RTMTypes where

import GHC.Generics (Generic)
import Control.Monad.Trans.Either
import Data.Aeson
import RTMTypesInternal
import Language.Haskell.TH.Syntax

newtype RTMResponse a = RTMResponse { rsp :: a } deriving (Generic, Show)
data ErrorInfo = ErrorInfo { code :: String, msg :: String } deriving (Generic, Show, Eq)
data RError = RError { stat :: String, err :: ErrorInfo} deriving (Generic, Show, Eq)
type Response a = EitherT ErrorInfo IO a

data AuthLevel = AuthRequired | SignatureRequired | NoAuthRequired



$(genRtmDataType (mkName "Transaction") [("id", strT), ("undoable", strT)])
$(genRtmDataType (mkName "User") [("id", strT), ("username", strT), ("fullname", strT)])
-- data User = User {id :: String, username :: String, fullname :: String} deriving (Show, Eq)
-- instance FromJSON User where
--     parseJSON = withObject "User" $ \v -> User
--         <$> v .: "id"
--         <*> v .: "username"
--         <*> v .: "fullname"
$(genRtmDataType (mkName "Auth") [("token", strT), ("perms", strT), ("user", ConT ''User)])
$(genRtmDataType (mkName "List") [("id", strT), ("name", strT), ("deleted", strT), ("locked", strT), ("archived", strT), ("position", strT), ("smart", strT)])
$(genRtmDataType (mkName "Lists") [("list", AppT ListT (ConT ''List))])
-- data List = List {id :: String, name :: String, deleted :: String, locked :: String, archived :: String, position :: String, smart :: String} deriving (Generic, Show, Eq)
-- instance FromJSON List
-- newtype Lists = Lists {list :: [List]} deriving (Generic, Show, Eq)
-- instance FromJSON Lists


instance FromJSON RError
instance FromJSON ErrorInfo
instance (FromJSON a) => FromJSON (RTMResponse a)