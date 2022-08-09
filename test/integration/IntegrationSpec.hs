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


assertFailOnLeft :: Either ErrorInfo b -> IO()
assertFailOnLeft val =
    case val of
        Left error -> assertFailure (msg error)
        _ -> return ()

spec :: IO ()
spec = do
    (key, secret) <- getApiKeyAndSharedSecret
    hspec $ do
        describe "RTMInterface.rtmEcho" $ do
            it "should return Text with supplied name" $ do
                let textToEcho = "hello!"
                echo <- runEitherT (rtmEcho key textToEcho)
                echo `shouldBe` Right textToEcho
        describe "RTMInterface.rtmFrob" $ do
            it "should return a RFrob object" $ do
                frobReturn <- runEitherT (rtmFrob key secret)
                assertFailOnLeft frobReturn
            it "should have nonempty Text payload" $ do
                frobReturn <- runEitherT (rtmFrob key secret)
                (null <$> frobReturn) `shouldBe` Right False
                -- TODO find a way to not have to get the key and secret on each test

specWithFrob :: Maybe T.Text -> IO ()
specWithFrob Nothing = hspec $ do
    describe "userAuth" $ do
        it "should generate a callback with a valid frob token" $ do
            assertFailure "no frob token in callback url" :: IO()
specWithFrob (Just frobT) = do
    (key, secret) <- getApiKeyAndSharedSecret
    let frob = T.unpack frobT
    hspec $ do
        describe "rtmGetToken" $ do
            it "should fetch a valid auth token for user" $ do
                auth <- runEitherT $ rtmGetToken key frob secret
                assertFailOnLeft auth