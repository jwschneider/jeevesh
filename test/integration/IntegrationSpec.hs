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
            let textToEcho = "hello!"
            echo <- runEitherT (rtmEcho textToEcho)
            echo `shouldBe` Right textToEcho
    describe "RTMInterface.rtmFrob" $ do
        it "should return a RFrob object" $ do
            frobReturn <- runEitherT rtmFrob
            assertFailOnLeft frobReturn
        it "should have nonempty Text payload" $ do
            frobReturn <- runEitherT rtmFrob
            (null <$> frobReturn) `shouldBe` Right False