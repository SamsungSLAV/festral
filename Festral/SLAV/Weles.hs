{-
 - Copyright (c) 2018 Samsung Electronics Co., Ltd All Rights Reserved
 -
 - Author: Uladzislau Harbuz <u.harbuz@samsung.com>
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -      http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |Simple library implementing SLAV Weles API, which allows low level test
-- management using Weles as testing server. It allows creating, cancelling,
-- waiting Weles jobs and processing it in different ways.
--
-- This library use module "Festral.Config" and "Festral.Files" for get server
-- IP, port etc.
module Festral.SLAV.Weles (
    Job(..),
    JobParameters(..),
    curlJobs,
    getJob,
    getJobWhenDone,
    startJob,
    getFileList,
    getJobOutFile,
    getJobOut,
    cancelAll,
    cancelJob
) where

import           Network.Curl.Aeson
import           Network.Curl
import           Control.Applicative
import           Data.Aeson
import           GHC.Generics
import           Control.Concurrent
import           System.Process
import           System.IO
import qualified Data.ByteString.Lazy   as LB
import qualified Data.ByteString.Char8  as B
import           Data.List.Split
import           Data.List
import           Control.Monad
import           Data.Maybe
import           System.Directory
import           Control.Exception

import           Festral.Files
import           Festral.Config

-- |Job datatype describes json job object got from weles
data Job = Job
    {jobid      :: Int
    ,name       :: String
    ,created    :: String
    ,updated    :: String
    ,status     :: String
    ,info       :: String
} deriving (Generic)

-- |Datatype incapsulating parameters of the job
data JobParameters
    = JobParameters {
    -- |Timeout of job from it was created. Job will be closed after it expired
    -- even if it just waiting all the time. It is in seconds.
      absoluteTimeout   :: Int
    -- |Timeout which starts from job finished waiting and started running. It
    -- has no sence to be longer than 'absoluteTimeout' value because
    -- 'absoluteTimeout' has higher priority. It is in seconds.
    , afterRunTimeout   :: Int
    }

instance Show JobParameters where
    show x =  "TTL: " ++ show (absoluteTimeout x)
           ++ " RunTTL: " ++ show (afterRunTimeout x)

instance FromJSON Job
instance ToJSON Job
instance Show Job where
    show (Job id n c u s i) = "{\n \"jobid\" : " ++ show id ++ ",\n \"name\" : "
                ++ show n ++ ",\n \"created\" : "
                ++ show c ++ ",\n \"updated\" : "
                ++ show u ++ ",\n \"status\" : "
                ++ show s ++ ",\n \"info\" : "
                ++ show i ++ "\n}\n"

welesAddr = do
    config <- getAppConfig
    return (welesIP config, welesPort config, welesFilePort config)

-- |Get list of all jobs on server
curlJobs :: IO [Job]
curlJobs = do
    (ip, port, _) <- welesAddr
    handle badCurl $ curlAesonGet (ip ++ ":" ++ show port ++ "/api/v1/jobs/")
    where
        badCurl :: CurlAesonException -> IO [Job]
        badCurl ex = putStrLn (show ex) >> return []

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
activeStatuses = ["RUNNING"]

-- |Wait until job with given id got status one of FAILED | COMPLETED | CANCELED
-- or until time limit not expared and then return this job.
getJobWhenDone :: Int -- ^ Job ID
               -> JobParameters -- ^ Timeouts to wait in seconds
               -> IO (Maybe Job)
getJobWhenDone id parameters = do
    job <- getJob id
    f job
    where
        f job
            | isNothing job || (status <$> job) `elem` (map Just doneStatuses)
                = return job
            | timeout <= 0 || runTTL <= 0
                = cancelJob id >> threadDelay oneSec
                >> getJobWhenDone id parameters
            | otherwise = threadDelay oneSec >>
                getJobWhenDone id (JobParameters (timeout - 1) newRunTTL)
            where
                newRunTTL = if (status <$> job) `elem` (map Just activeStatuses)
                                then runTTL - 1
                                else runTTL
        timeout = absoluteTimeout parameters
        runTTL = afterRunTimeout parameters

data SimpleJob = SimpleJob {s_jobid :: Int}
    deriving (Show)

instance FromJSON SimpleJob where
    parseJSON = withObject "SimpleJob" $ \v -> SimpleJob <$> v.: "jobid"

-- |Send new job defined in the YAML file defined as parameter to server.
-- Returns id of new job.
startJob :: FilePath -> IO (Maybe Int)
startJob yamlFileName = do
    (ip, port, _) <- welesAddr
    (_, out, err, _) <- runInteractiveCommand (
                        "curl -sL "
                         ++ ip
                         ++ ":"
                         ++ show port
                         ++ "/api/v1/jobs/ -F \"uploadfile=@"
                         ++ yamlFileName
                         ++ ";\""
                      )
    outStr <- hGetContents out
    let sjob = decode (LB.fromStrict $ B.pack outStr) :: Maybe SimpleJob
    return (s_jobid <$> sjob)

testFileUrl id = do
    (ip, _, port) <- welesAddr
    return $ ip ++ ":" ++ show port ++ "/" ++ show id ++ "/TESTFILE/"

-- |Returns list of filenames generated by job with given id.
-- If job with given id does not exists return Nothing.
getFileList :: Int -> IO (Maybe [String])
getFileList id = do
    fileUrl <- testFileUrl id
    (errCode, htmlOut) <- curlGetString fileUrl [CurlFollowLocation True]
    let res = if errCode == CurlOK
            then Just (extractHrefs htmlOut)
            else Nothing
    return res

    where
        extractHrefs = (map (\x -> x!!1)) . (map (splitOn "\"")) .
            ((filter (isInfixOf "a href=")) . (splitOneOf "<>"))

-- |Returns just contents of the file with given name located on Weles server
-- for job with given id. If file or job does not exists returns Nothing.
getJobOutFile :: Int -> String -> IO (Maybe String)
getJobOutFile id fname = do
    fileUrl <- testFileUrl id
    (errCode, content) <- curlGetString (fileUrl ++ fname)
        [CurlFollowLocation True]
    let res = if errCode == CurlOK
            then Just content
            else Nothing
    return res

-- |Returns standard output of job given by id (contents of the results file).
getJobOut :: Int -> IO String
getJobOut id = do
    fnames <- getFileList id
    let resFnames = filter (isInfixOf "results") <$> fnames
    let Just resName = if resFnames == Nothing
            then Just []
            else resFnames
    let contents = map (getJobOutFile id) resName
    concat <$> (liftM concat) <$> (sequence contents)

-- |Cancel job defined by ID
cancelJob :: Int -> IO ()
cancelJob id = do
    (ip, port, _) <- welesAddr
    curlPost (ip ++ ":" ++ show port ++ "/api/v1/jobs/"
        ++ show id ++ "/cancel") []

-- |Cancel all not done Weles jobs.
cancelAll :: IO ()
cancelAll = do
    jobs <- curlJobs
    let runningJobs = filter (\ x -> not $ (status x) `elem` doneStatuses) jobs
    mapM_ (\ x -> cancelJob (jobid x)) runningJobs

oneSec = 1000000
