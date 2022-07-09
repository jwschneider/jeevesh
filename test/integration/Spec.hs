{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import RTMInterface
import Data.Text
import Control.Monad.Trans.Either
import GHC.TypeLits


main :: IO ()
main = hspec $ do
    describe "RTMInterface.rtmEcho" $ do
        it "should return an REcho object with supplied name" $ do
            let textToEcho = "hello!" :: Text
            echo <- runEitherT (rtmEcho textToEcho)
            (name <$> echo) `shouldBe` Right textToEcho