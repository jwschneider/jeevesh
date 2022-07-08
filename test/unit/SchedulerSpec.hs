{-# LANGUAGE OverloadedStrings #-}

module SchedulerSpec where

import Data.Aeson
import Data.String
import qualified Data.ByteString.Lazy as B
import Test.Hspec
import Scheduler

instance FromJSON Job where
    parseJSON = withObject "Job" $ \v -> Job
        <$> v .: "identifier"
        <*> v .: "releaseDate"
        <*> v .: "processTime"
        <*> v .: "deadline"
        <*> v .: "value"

sampleSetup :: SetupTime
sampleSetup i j = case (identifier <$> i, identifier j) of
                    (Nothing, "4") -> 2
                    (Just "4", "1") -> 2
                    (Just "4", "3") -> 2
                    _ -> 1

sampleSchedule :: IO (Maybe Schedule)
sampleSchedule = do
    jsonString <- B.readFile "test/unit/jobs/SampleSchedulerData.json"
    let jobs = decode jsonString :: Maybe [Job]
    return $ scheduleJobs <$> jobs <*> Just sampleSetup

spec :: Spec
spec = do
    describe "Scheduler.Job parseJSON" $ do
        it "decodes a Job in correct JSON representation" $ do
            jsonString <- B.readFile "test/unit/jobs/SampleGoodJob.json"
            (decode jsonString :: Maybe Job) `shouldBe` Just (Job "0" 0 0 0 0)
        it "returns Nothing otherwise" $ do
            jsonString <- B.readFile "test/unit/jobs/SampleBadJob.json"
            (decode jsonString :: Maybe Job) `shouldBe` Nothing
    describe "Scheduler.Schedule on SampleSchedulerData" $ do
        it "has correct Job order" $ do
            schedule <- sampleSchedule
            let jobOrder = map (identifier . fst) <$> schedule
            jobOrder `shouldBe` Just ["2", "3", "4"]
        it "has correct launch times" $ do
            schedule <- sampleSchedule
            let launchTimes = map snd <$> schedule
            launchTimes `shouldBe` Just [2, 5, 9]
