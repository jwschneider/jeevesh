module Scheduler where

import Data.Maybe
import Data.List
import Data.Function

data Job = 
    Job {
        identifier :: String,
        releaseDate :: Int,
        processTime :: Int,
        deadline :: Int,
        value :: Int
    } deriving (Eq, Show)

processTimeM :: Maybe Job -> Int
processTimeM = maybe 0 processTime



type SetupTime = Maybe Job -> Job -> Int
exampleSetup :: SetupTime
exampleSetup i j = 0

type JobsRemaining = Maybe Job -> Int -> [Job]
type Schedule = [(Job, Int)]
type CandidateSchedule = (Int, Schedule)



highestValueAndEarliestCompletion :: CandidateSchedule -> CandidateSchedule -> Ordering
highestValueAndEarliestCompletion candidateA candidateB = 
    let cmp = (compare `on` fst) candidateA candidateB in
        if cmp == EQ
            then (compare `on` negate . completionTime . snd) candidateA candidateB
            else cmp

completionTime :: Schedule -> Int
completionTime = (processTime . fst . last) `add` (snd . last) 

add :: (a -> Int) -> (a -> Int) -> a -> Int
add f g x = f x + g x

(!\) :: Eq a => [a] -> [a] -> [a]
(!\) x y  = filter (`notElem` y) x

scheduleRemainingJobs :: Maybe Job -> [Job] -> Int -> JobsRemaining -> SetupTime -> (Int, Schedule)
scheduleRemainingJobs i x t_i f setup =
    let jobsRemaining = f i t_i !\ x in
        bestCandidateRemainingSchedule jobsRemaining where
            bestCandidateRemainingSchedule [] = (0, [])
            bestCandidateRemainingSchedule jobsRemaining = maximumBy highestValueAndEarliestCompletion $ map buildRemainingSchedule jobsRemaining where
                buildRemainingSchedule k = 
                    let p_i = processTimeM i
                        t_k = max (t_i + p_i + setup i k) (releaseDate k)
                        x' = k : x
                        x_k = filter (\j -> deadline j - processTime j >= t_k + processTime k + setup (Just k) j) x'
                        (v_j, remainingSchedule) = scheduleRemainingJobs (Just k) x_k t_k f setup
                    in (value k + v_j, (k, t_k) : remainingSchedule)

scheduleJobs :: [Job] -> SetupTime -> Schedule
scheduleJobs jobs setup = snd $ scheduleRemainingJobs Nothing [] 0 f setup where
    f i t_i = 
        let p_i = processTimeM i
        in filter (\j -> t_i + p_i + setup i j <= deadline j - processTime j) jobs