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


$(genRtmDataType (mkName "Transaction") [("id", ''String), ("undoable", ''String)])
$(genRtmDataType (mkName "User") [("id", ''String), ("username", ''String), ("fullname", ''String)])
-- data User = User {id :: String, username :: String, fullname :: String} deriving (Show, Eq)
-- instance FromJSON User where
--     parseJSON = withObject "User" $ \v -> User
--         <$> v .: "id"
--         <*> v .: "username"
--         <*> v .: "fullname"
$(genRtmDataType (mkName "Auth") [("token", ''String), ("perms", ''String), ("user", ''User)])
-- data List = List {id :: String, name :: String, deleted :: String, locked :: String, archived :: String, position :: String, smart :: String} deriving (Generic, Show, Eq)
-- instance FromJSON List
-- newtype Lists = Lists {list :: [List]} deriving (Generic, Show, Eq)
-- instance FromJSON Lists


instance FromJSON RError
instance FromJSON ErrorInfo
instance (FromJSON a) => FromJSON (RTMResponse a)