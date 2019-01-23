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
-- In this modole, 'NetAddress' values contains ip address of the Weles server,
-- its REST API port number and its file server port number. These addresses
-- should be made by 'makeAddress' function.
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

import Network.Curl.Aeson
import Network.Curl
import Control.Applicative
import Data.Aeson
import GHC.Generics
import Control.Concurrent
import System.Process
import System.IO
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B
import Data.List.Split
import Data.List
import Control.Monad
import Data.Maybe
import System.Directory
import Control.Exception

import Festral.Config

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

welesAddr x = (netIP x, netPort x, netFilePort x)

-- |Get list of all jobs on server under given address.
curlJobs :: NetAddress -> IO [Job]
curlJobs addr = do
    let (ip, port, _) = welesAddr addr
    handle badCurl $ curlAesonGet (ip ++ ":" ++ show port ++ "/api/v1/jobs/")
    where
        badCurl :: CurlAesonException -> IO [Job]
        badCurl ex = putStrLn (show ex) >> return []

-- |Get job by its ID.
getJob :: NetAddress -> Int -> IO (Maybe Job)
getJob addr id = do
    jobs <- curlJobs addr
    let job = filter ((id ==) . jobid) jobs
    let res = if length job == 0
                then Nothing
                else Just $ head job
    return res

doneStatuses = ["FAILED", "COMPLETED", "CANCELED"]
activeStatuses = ["RUNNING"]

-- |Wait until job with given id got status one of FAILED | COMPLETED | CANCELED
-- or until time limit not expared and then return this job.
getJobWhenDone :: NetAddress    -- ^ Address of Weles server
               -> Int           -- ^ Job ID
               -> JobParameters -- ^ Timeouts to wait in seconds
               -> IO (Maybe Job)
getJobWhenDone addr id parameters = do
    job <- getJob addr id
    f job
    where
        f job
            | isNothing job || (status <$> job) `elem` (map Just doneStatuses)
                = return job
            | timeout <= 0 || runTTL <= 0
                = cancelJob addr id >> threadDelay oneSec
                >> getJobWhenDone addr id parameters
            | otherwise = threadDelay oneSec >>
                getJobWhenDone addr id (JobParameters (timeout - 1) newRunTTL)
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
startJob :: NetAddress -> FilePath -> IO (Maybe Int)
startJob addr yamlFileName = do
    let (ip, port, _) = welesAddr addr
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

testFileUrl addr id = do
    let (ip, _, port) = welesAddr addr
    return $ ip ++ ":" ++ show port ++ "/" ++ show id ++ "/TESTFILE/"

-- |Returns list of filenames generated by job with given id.
-- If job with given id does not exists return Nothing.
getFileList :: NetAddress -> Int -> IO (Maybe [String])
getFileList addr id = do
    fileUrl <- testFileUrl addr id
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
getJobOutFile :: NetAddress -> Int -> String -> IO (Maybe String)
getJobOutFile addr id fname = do
    fileUrl <- testFileUrl addr id
    (errCode, content) <- curlGetString (fileUrl ++ fname)
        [CurlFollowLocation True]
    let res = if errCode == CurlOK
            then Just content
            else Nothing
    return res

-- |Returns standard output of job given by id (contents of the results file).
getJobOut :: NetAddress -> Int -> IO String
getJobOut addr id = do
    fnames <- getFileList addr id
    let resFnames = filter (isInfixOf "results") <$> fnames
    let Just resName = if resFnames == Nothing
            then Just []
            else resFnames
    let contents = map (getJobOutFile addr id) resName
    concat <$> (liftM concat) <$> (sequence contents)

-- |Cancel job defined by ID
cancelJob :: NetAddress -> Int -> IO ()
cancelJob addr id = do
    let (ip, port, _) = welesAddr addr
    curlPost (ip ++ ":" ++ show port ++ "/api/v1/jobs/"
        ++ show id ++ "/cancel") []

-- |Cancel all not done Weles jobs.
cancelAll :: NetAddress -> IO ()
cancelAll addr = do
    jobs <- curlJobs addr
    let runningJobs = filter (\ x -> not $ (status x) `elem` doneStatuses) jobs
    mapM_ (\ x -> cancelJob addr (jobid x)) runningJobs

oneSec = 1000000
