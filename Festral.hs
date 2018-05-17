{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Curl.Aeson
import Control.Applicative
import Data.Aeson
import GHC.Generics
import Control.Concurrent
import System.Process
import System.IO
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B
import System.Environment

main = do
    args <- getArgs
    runCmd args

runCmd :: [String] -> IO ()
runCmd ["allJobs"] = show <$> curlJobs >>= putStrLn
runCmd ["getJob", y] = do
    let id = read y :: Int
    show <$> getJob id >>= putStrLn
runCmd ["getJobWhenDone", y] = do
    let id = read y :: Int
    show <$> getJobWhenDone id >>= putStrLn
runCmd ["startJob", f] = show <$> startJob f >>= putStrLn
runCmd _ = do
    pname <- getProgName
    putStrLn $ "Usage: " ++ pname ++ "<allJobs | getJob id | getJobWhenDone id | startJob yamlname>" 


-- |Job datatype describes json job object got from weles
data Job = Job {
    jobid   :: Int
    ,name   :: String
    ,created:: String
    ,updated:: String
    ,status :: String
    ,info   :: String
} deriving (Show, Generic)

instance FromJSON Job 
instance ToJSON Job 

welesAddr = "127.0.0.1"
welesPort = "5010"

-- |Get list of all jobs on server
curlJobs :: IO [Job]
curlJobs = curlAesonGet (welesAddr ++ ":" ++ welesPort ++ "/api/v1/jobs/")

-- |Get job by its ID
getJob :: Int -> IO (Maybe Job)
getJob id = do
    jobs <- curlJobs
    let job = filter ((id ==) . jobid) jobs
    let res = if length job == 0
                then Nothing
                else Just $ head job
    return res

doneStatuses = ["FAILED", "COMPLETED", "CANCELED"]

-- |Wait until job with given id got status one of 'doneStatuses' and then return this job
getJobWhenDone :: Int -> IO (Maybe Job)
getJobWhenDone id = do
    job <- getJob id
    let res = if (status <$> job) `elem` (map Just doneStatuses)
                then return job
                else threadDelay 1000000 >> getJobWhenDone id
    res

data SimpleJob = SimpleJob {s_jobid :: Int}
    deriving (Show)

instance FromJSON SimpleJob where
    parseJSON = withObject "SimpleJob" $ \v -> SimpleJob <$> v.: "jobid"

-- |Send new job defined in the YAML file with name given as parameter to server
-- Returns id of new task
startJob :: String -> IO (Maybe Int)
startJob yamlFileName = do
    (_, out, err, _) <- runInteractiveCommand (
                        "curl -sL " 
                         ++ welesAddr 
                         ++ ":" 
                         ++ welesPort 
                         ++ "/api/v1/jobs/ -F \"uploadfile=@"
                         ++ yamlFileName
                         ++ ";\""
                      )
    outStr <- hGetContents out
    let sjob = decode (LB.fromStrict $ B.pack outStr) :: Maybe SimpleJob
    putStrLn =<< hGetContents err
    return (s_jobid <$> sjob)
