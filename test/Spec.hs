{-# LANGUAGE OverloadedStrings #-}

import Scheduler
import RTMInterface

import Data.Aeson
import Data.String
import qualified Data.ByteString.Lazy as B
import Test.HUnit
import System.Exit

instance FromJSON Job where
    parseJSON = withObject "Job" $ \v -> Job
        <$> v .: "identifier"
        <*> v .: "releaseDate"
        <*> v .: "processTime"
        <*> v .: "deadline"
        <*> v .: "value"


parseJsonContainsJob :: Bool -> Job -> Test
parseJsonContainsJob contains job = TestCase(do
                                                jsonString <- B.readFile "test/SampleSchedulerData.json"
                                                let jobs = decode jsonString :: Maybe [Job]
                                                assertEqual ("SampleData contains " ++ show job ++ " should be " ++ show contains) (Just contains) $ fmap (elem job) jobs)

runAssertOnSampleSchedule :: SetupTime -> (Maybe Schedule -> a -> Assertion) -> a -> Test
runAssertOnSampleSchedule setup assert expected = TestCase(do
                                    jsonString <- B.readFile "test/SampleSchedulerData.json"
                                    let jobs = decode jsonString :: Maybe [Job]
                                    let sampleSchedule = scheduleJobs <$> jobs <*> Just setup
                                    assert sampleSchedule expected)

sampleSetup :: SetupTime
sampleSetup i j = case (identifier <$> i, identifier j) of
                    (Nothing, "4") -> 2
                    (Just "4", "1") -> 2
                    (Just "4", "3") -> 2
                    _ -> 1

assertExpectedOrder :: Maybe Schedule -> [String] -> Assertion
assertExpectedOrder schedule expectedOrder = 
    assertEqual ("Order of jobs should be " ++ show expectedOrder) (Just expectedOrder) $ map (identifier . fst) <$> schedule

assertLaunchTimes :: Maybe Schedule -> [Int] -> Assertion
assertLaunchTimes schedule expectedTimes =
    assertEqual ("Launch times of jobs should be " ++ show expectedTimes) (Just expectedTimes) $ map snd <$> schedule



tests = TestList [
    TestLabel "parseJSON_SampleSchedulerData.json_containsJob1" $ parseJsonContainsJob True (Job "1" 0 3 8 2),
    TestLabel "parseJSON_SampleSchedulerData.json_doesNotContainBadJob1" $ parseJsonContainsJob False (Job "1" 0 3 8 1),
    TestLabel "scheduleJobs_SampleSchedulerData.json_expectedOrder" $ runAssertOnSampleSchedule sampleSetup assertExpectedOrder ["2", "3", "4"],
    TestLabel "scheduleJobs_SampleSchedulerData.json_expectedLaunchTimes" $ runAssertOnSampleSchedule sampleSetup assertLaunchTimes [2, 5, 9]
    ]


main :: IO ()
main = do
    results <- runTestTT tests
    if errors results + failures results == 0 then
        exitSuccess
    else die "test suite failed"

  
        



--decodeJSArray :: String -> [Job]
-- make Job a JSON Instance