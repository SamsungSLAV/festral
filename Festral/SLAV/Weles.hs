{-
 - Copyright (c) 2018-2019 Samsung Electronics Co., Ltd All Rights Reserved
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

-- |Simple library implementing SLAV Weles API, which allows low level test
-- management using Weles as testing server. It allows creating, cancelling,
-- waiting Weles jobs and processing it in different ways.
--
-- In this modole, 'NetAddress' values contains ip address of the Weles server,
-- its REST API port number and its file server port number. These addresses
-- should be made by 'makeAddress' function.
module Festral.SLAV.Weles (
    curlJobs,
    getJob,
    getJobWhenDone,
    startJob,
    getFileList,
    getJobOutFile,
    getJobOut,
    cancelAll,
    cancelJob,
    getAPIVersion,
    getJobYaml,
    module Festral.SLAV.Weles.Data,
    module Old,
    module V1,
    module V2
) where

import Network.Curl.Aeson
import Network.Curl
import Control.Concurrent
import System.Process
import System.IO
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B
import Data.List.Split
import Data.List
import Control.Monad
import System.Directory
import Control.Exception
import Text.Read
import Data.Aeson
import Data.Maybe

import Festral.Config
import Festral.SLAV.Weles.Data
import qualified Festral.SLAV.Weles.API.Old as Old
import qualified Festral.SLAV.Weles.API.V1  as V1
import qualified Festral.SLAV.Weles.API.V2  as V2
import Festral.SLAV.Weles.API.V1 (getArtifacts, filterID,
                                  filterName, filterEmpty)

welesAddr x = (netIP x, netPort x, netFilePort x)

-- |Get list of all jobs on server under given address. This function use v1 API
-- of veles and replaces 'curlJobs' function.
curlJobs :: NetAddress -> IO [Job]
curlJobs addr = do
    apiVersion <- getAPIVersion addr
    case version <$> apiVersion of
        (Just "0.2.0") -> V2.curlJobs addr
        (Just "0.1.0") -> V1.curlJobs addr
        _              -> Old.curlJobs addr

-- |Get job by its ID.
getJob :: NetAddress -> Int -> IO (Maybe Job)
getJob addr id = do
    jobs <- curlJobs addr
    let job = filter ((id ==) . jobid) jobs
    let res = if length job == 0
                then Nothing
                else Just $ head job
    return res

-- |Returns just version of the Weles API or 'Nothing' if API is too old or
-- other connection error occured.
--
-- @since 1.3.0
getAPIVersion :: NetAddress -> IO (Maybe APIVersion)
getAPIVersion addr = do
    let (ip, port, _) = welesAddr addr
    handle badCurl $ curlAesonGet (ip ++ ":" ++ show port ++ "/api/v1/version")
    where
        badCurl :: CurlAesonException -> IO (Maybe APIVersion)
        badCurl _ = return Nothing

-- |Get YAML file of the job specified by ID. This function works only with
-- Weles API v1 and later.
--
-- @since 1.3.3
getJobYaml :: NetAddress -> Int -> IO (Maybe String)
getJobYaml = V1.getJobYaml

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

-- |Send new job defined in the YAML file defined as parameter to server.
-- Returns id of new job.
startJob :: NetAddress -> FilePath -> IO (Maybe Int)
startJob addr yamlFileName = do
    apiVersion <- getAPIVersion addr
    let filenameArg = resolveFileArg apiVersion
    let (ip, port, _) = welesAddr addr
    (_, out, err, _) <- runInteractiveCommand (
                        "curl -sL "
                         ++ ip
                         ++ ":"
                         ++ show port
                         ++ "/api/v1/jobs/ -F \""
                         ++ filenameArg ++ "=@"
                         ++ yamlFileName
                         ++ ";\""
                      )
    outStr <- hGetContents out
    return $ parseF apiVersion outStr
    where
        resolveFileArg Nothing = "uploadfile"
        resolveFileArg _ = "yamlfile"

        parseF Nothing x = s_jobid
            <$> (decode (LB.fromStrict $ B.pack x) :: Maybe SimpleJob)
        parseF _ x = readMaybe x :: Maybe Int

-- |Returns list of filenames generated by job with given id.
-- If job with given id does not exists return Nothing.
getFileList :: NetAddress -> Int -> IO (Maybe [String])
getFileList addr id = do
    apiVersion <- getAPIVersion addr
    (apiDependFileList $ version <$> apiVersion) addr id
    where
        apiDependFileList (Just "0.2.0") = V2.getFileList
        apiDependFileList (Just "0.1.0") = V1.getFileList
        apiDependFileList _              = Old.getFileList

genericJobOutFile fileUrl = do
    (errCode, content) <- curlGetString fileUrl [CurlFollowLocation True]
    let res = if errCode == CurlOK
            then Just content
            else Nothing
    return res

-- |Returns just contents of the file with given name located on Weles server
-- for job with given id. If file or job does not exists returns Nothing.
--
-- @since 1.3.2
getJobOutFile :: NetAddress -> Int -> String -> IO (Maybe String)
getJobOutFile addr id fname = do
    apiVersion <- getAPIVersion addr
    (apiJobOutFile $ version <$> apiVersion) addr id fname
    where
        apiJobOutFile (Just "0.2.0") = V2.getJobOutFile
        apiJobOutFile (Just "0.1.0") = V1.getJobOutFile
        apiJobOutFile _              = Old.getJobOutFile

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
