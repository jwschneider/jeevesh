{-# LANGUAGE OverloadedStrings #-}

module IntegrationSpec where

import Test.HUnit
import Test.Hspec
import RTMTypes
import RTMInterface
import qualified Data.Text as T
import Control.Monad.Trans.Either
import GHC.TypeLits


assertFailOnLeft :: Either RError b -> IO()
assertFailOnLeft val =
    case val of
        Left error -> assertFailure ((T.unpack . msg . err) error)
        _ -> return ()

spec :: Spec
spec = do
    describe "RTMInterface.rtmEcho" $ do
        it "should return Text with supplied name" $ do
            let textToEcho = "hello!"
            echo <- runEitherT (rtmEcho textToEcho)
            echo `shouldBe` Right (T.pack textToEcho)
    describe "RTMInterface.rtmFrob" $ do
        it "should return a RFrob object" $ do
            frobReturn <- runEitherT rtmFrob
            assertFailOnLeft frobReturn
        it "should have nonempty Text payload" $ do
            frobReturn <- runEitherT rtmFrob
            (T.null <$> frobReturn) `shouldBe` Right False