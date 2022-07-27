{-# LANGUAGE OverloadedStrings #-}

module IntegrationSpec where

import Test.HUnit
import Test.Hspec
import RTMTypes
import RTMInterface
import RTMApi
import qualified Data.Text as T
import Control.Monad.Trans.Either
import GHC.TypeLits


assertFailOnLeft :: Either RError b -> IO()
assertFailOnLeft val =
    case val of
        Left error -> assertFailure ((msg . err) error)
        _ -> return ()

spec :: Spec
spec = do
    describe "RTMInterface.rtmEcho" $ do
        it "should return Text with supplied name" $ do
            (key, secret) <- getApiKeyAndSharedSecret
            let textToEcho = "hello!"
            echo <- runEitherT (rtmEcho key textToEcho)
            echo `shouldBe` Right textToEcho
    describe "RTMInterface.rtmFrob" $ do
        it "should return a RFrob object" $ do
            (key, secret) <- getApiKeyAndSharedSecret
            frobReturn <- runEitherT (rtmFrob key secret)
            assertFailOnLeft frobReturn
        it "should have nonempty Text payload" $ do
            (key, secret) <- getApiKeyAndSharedSecret
            frobReturn <- runEitherT (rtmFrob key secret)
            (null <$> frobReturn) `shouldBe` Right False
            -- TODO find a way to not have to get the key and secret on each test