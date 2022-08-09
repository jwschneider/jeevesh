{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import IntegrationSpec
import RTMInterface
import RTMApi

import Yesod
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Web.Browser (openBrowser)
import Data.List (foldl', foldr)


data TestingServer = TestingServer

mkYesod "TestingServer" [parseRoutes|
/test/integration TestIntegrationR GET
|]

instance Yesod TestingServer

getTestIntegrationR :: Handler Html
getTestIntegrationR = do
    frob <- lookupGetParam "frob"
    liftIO $ specWithFrob frob
    defaultLayout [whamlet|Please close browser to finish tests|]

launchBrowserForAuth :: IO ()
launchBrowserForAuth = do
    (key, secret) <- getApiKeyAndSharedSecret
    let callback = "http://localhost:3000/test/integration"
    userAuthRedirect key callback secret

launchServerForAuthTests :: IO ()
launchServerForAuthTests = do
    app <- toWaiApp TestingServer
    toDie <- newEmptyMVar
    let settings = [setTimeout 5,
                    setOnClose (\sock -> putMVar toDie ()),
                    setPort 3000]
    let runServer = runSettings (foldr ($) defaultSettings settings) app
    race_ (takeMVar toDie) runServer 


main :: IO ()
main = do
    spec
    withAsync launchServerForAuthTests $ \server -> do
        launchBrowserForAuth
        wait server
        putStrLn "Server shut down successfully"