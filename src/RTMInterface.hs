{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module RTMInterface where

import RTMTypes
import Language.Haskell.TH.Syntax
import qualified Data.Text as T



-- rtmEcho :: T.Text -> Response REcho
-- rtmEcho name = EitherT $ do
--     body <- rtmGetJ [("method", "rtm.test.echo"), ("name", name)]
--     return (parseResponse body :: Either RError REcho)
$(genRtmMethod "Echo" "rtm.test.echo" "name" ''T.Text ["name"])
$(genRtmMethod "Frob" "rtm.auth.getFrob" "frob" ''T.Text [])

