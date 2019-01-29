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
    module Festral.SLAV.Weles.Data
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

welesAddr x = (netIP x, netPort x, netFilePort x)

-- |Get list of all jobs on server under given address.
curlJobsOld :: NetAddress -> IO [Job]
curlJobsOld addr = do
    let (ip, port, _) = welesAddr addr
    handle badCurl $ curlAesonGet (ip ++ ":" ++ show port ++ "/api/v1/jobs/")
    where
        badCurl :: CurlAesonException -> IO [Job]
        badCurl ex = putStrLn (show ex) >> return []

-- |Get list of all jobs on server under given address. This function use v1 API
-- of veles and replaces 'curlJobs' function.
--
-- @since 1.3.0
curlJobs :: NetAddress -> IO [Job]
curlJobs addr = do
    let (ip, port, _) = welesAddr addr
    handle (useV0API addr) $ curlAeson
        parseJSON
        "POST"
        (ip ++ ":" ++ show port ++ "/api/v1/jobs/list")
        [CurlFollowLocation True]
        (Nothing :: Maybe Job)

useV0API :: NetAddress -> CurlAesonException -> IO [Job]
useV0API addr e = curlJobsOld addr

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

-- |Get url for artifact storage in V0 Weles API for job specified by id.
testFileUrl addr id =
    let (ip, _, port) = welesAddr addr in
        ip ++ ":" ++ show port ++ "/" ++ show id ++ "/TESTFILE/"

-- |Get url for download artifact by given id using V1 Weles API.
testFileUrlV1 addr id =
    let (ip, port, _) = welesAddr addr in
        ip ++ ":" ++ show port ++ "/api/v1/artifacts/" ++ show id

-- |Implementation of the old Weles API
getFileListOld :: NetAddress -> Int -> IO (Maybe [String])
getFileListOld addr id = do
    let fileUrl = testFileUrl addr id
    (errCode, htmlOut) <- curlGetString fileUrl [CurlFollowLocation True]
    let res = if errCode == CurlOK
            then Just (extractHrefs htmlOut)
            else Nothing
    return res
    where
        extractHrefs = (map (\x -> x!!1)) . (map (splitOn "\"")) .
            ((filter (isInfixOf "a href=")) . (splitOneOf "<>"))

-- |Get names of files of job specified by id. Always returns Just.
--
-- @since 1.3.2
getFileListV1 :: NetAddress -> Int -> IO (Maybe [String])
getFileListV1 addr id
    = Just . (a_alias <$>) <$> getArtifacts addr (filterID id)

-- |Returns list of filenames generated by job with given id.
-- If job with given id does not exists return Nothing.
getFileList :: NetAddress -> Int -> IO (Maybe [String])
getFileList addr id = do
    apiVersion <- getAPIVersion addr
    apiDependFileList apiVersion addr id
    where
        apiDependFileList Nothing = getFileListOld
        apiDependFileList _ = getFileListV1

-- |Get list of artifacts of Weles job specified by filter.
--
-- @since 1.3.2
getArtifacts :: NetAddress -> ArtifactFilter -> IO [Artifact]
getArtifacts addr filter = do
    let (ip, port, _) = welesAddr addr
    handle badCurl $ curlAeson
        parseJSON
        "POST"
        (ip ++ ":" ++ show port ++ "/api/v1/artifacts/list")
        [CurlFollowLocation True]
        (Just filter)
    where
        badCurl :: CurlAesonException -> IO [Artifact]
        badCurl _ = return []

-- |Filter artifacts by job id.
filterID id = ArtifactFilter $ emptyArtifact{a_jobid = id}

-- |Filter artifacts by its alias.
filterName name = ArtifactFilter $ emptyArtifact{a_alias = name}

-- |Do not filter artifacts.
filterEmpty = ArtifactFilter $ emptyArtifact

-- |Implementation of 'getJobOutFile' for Weles API V0
getJobOutFileOld :: NetAddress -> Int -> String -> IO (Maybe String)
getJobOutFileOld addr id fname = do
    let fileUrl = testFileUrl addr id
    genericJobOutFile $ fileUrl ++ fname

-- |Implementation of 'getJobOutFile' for Weles API V1.
--
-- @since 1.3.2
getJobOutFileV1 :: NetAddress -> Int -> String -> IO (Maybe String)
getJobOutFileV1 addr id fname = do
    artifacts <- getArtifacts addr $ filterID id <||> filterName fname
    let fileUrl = testFileUrlV1 addr $ a_id
                $ fromMaybe emptyArtifact $ listToMaybe artifacts
    genericJobOutFile fileUrl

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
    (apiJobOutFile apiVersion) addr id fname
    where
        apiJobOutFile Nothing = getJobOutFileOld
        apiJobOutFile _ = getJobOutFileV1

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
