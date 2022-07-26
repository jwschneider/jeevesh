
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}


module RTMTypes where

import GHC.Generics (Generic)
import Control.Monad.Trans.Either
import Data.Aeson



newtype RTMResponse a = RTMResponse { rsp :: a } deriving (Generic, Show)
data ErrorInfo = ErrorInfo { code :: String, msg :: String } deriving (Generic, Show, Eq)
data RError = RError { stat :: String, err :: ErrorInfo} deriving (Generic, Show, Eq)
type Response a = EitherT RError IO a

data AuthLevel = AuthRequired | SignatureRequired | NoAuthRequired

data User = User {id :: String, username :: String, fullname :: String} deriving (Generic, Show, Eq)
instance FromJSON User
data Auth = Auth {token :: String, perms :: String, user :: User} deriving (Generic, Show, Eq)
instance FromJSON Auth
data List = List {id :: String, name :: String, deleted :: String, locked :: String, archived :: String, position :: String, smart :: String} deriving (Generic, Show, Eq)
instance FromJSON List
newtype Lists = Lists {list :: [List]} deriving (Generic, Show, Eq)
instance FromJSON Lists


instance FromJSON RError
instance FromJSON ErrorInfo
instance (FromJSON a) => FromJSON (RTMResponse a)