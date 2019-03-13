{-
 - Copyright (c) 2019 Samsung Electronics Co., Ltd All Rights Reserved
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

-- |Implementation of V1 Weles API which is differs from other versions.
--
-- @since 1.5.0
module Festral.SLAV.Weles.API.V1 (
    curlJobs,
    getFileList,
    getJobOutFile,
    getArtifacts,
    getJobYaml,
    filterID,
    filterName,
    filterEmpty
) where

import Network.Curl.Aeson
import Network.Curl
import Data.List.Split
import Data.List
import Control.Exception
import Data.Aeson
import Data.Maybe

import Festral.Config
import Festral.SLAV.Weles.Data
import qualified Festral.SLAV.Weles.API.Old as Old

welesAddr x = (netIP x, netPort x)

-- |Get list of all jobs on server under given address. This function use v1 API
-- of weles and replaces 'curlJobs' function.
curlJobs :: NetAddress -> IO [Job]
curlJobs addr = do
    let (ip, port) = welesAddr addr
    handle badCurl $ curlAeson
        parseJSON
        "POST"
        (ip ++ ":" ++ show port ++ "/api/v1/jobs/list")
        [CurlFollowLocation True]
        (Nothing :: Maybe Job)
    where
        badCurl :: CurlAesonException -> IO [Job]
        badCurl _ = return []

-- |Get url for download artifact by given id using V1 Weles API.
testFileUrl addr id =
    let (ip, port) = welesAddr addr in
        ip ++ ":" ++ show port ++ "/" ++ show id ++ "/RESULT/"

-- |Get names of files of job specified by id. Always returns Just.
getFileList :: NetAddress -> Int -> IO (Maybe [String])
getFileList = Old.getFileList

-- |Get list of artifacts of Weles job specified by filter.
getArtifacts :: NetAddress -> ArtifactFilter -> IO [Artifact]
getArtifacts addr filter = do
    let (ip, port) = welesAddr addr
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

-- |Get YAML file of the job specified by ID. This function works only with
-- Weles API v1 and later.
--
-- @since 1.3.3
getJobYaml :: NetAddress -> Int -> IO (Maybe String)
getJobYaml addr id = do
    yamlID <- listToMaybe <$> (getArtifacts addr
        $ (filterID id) <&&> (ArtifactFilter $ emptyArtifact {a_type = "YAML"}))
    maybe (return Nothing) genericJobOutFile
        $ (testFileUrl addr) <$> a_id <$> yamlID

-- |Implementation of 'getJobOutFile' for Weles API V1.
getJobOutFile :: NetAddress -> Int -> String -> IO (Maybe String)
getJobOutFile = Old.getJobOutFile

genericJobOutFile fileUrl = do
    (errCode, content) <- curlGetString fileUrl [CurlFollowLocation True]
    let res = if errCode == CurlOK
            then Just content
            else Nothing
    return res
