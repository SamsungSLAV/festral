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

-- |Implementation of V2 Weles API which is differs from other versions.
--
-- V2 API is not released yet, but it is known that V1 API will not fix error
-- with getting files, so V1 functions are moved to the old model
-- of getting files.
--
-- @since 1.5.0
module Festral.SLAV.Weles.API.V2 (
    curlJobs,
    getFileList,
    getJobOutFile,
    getArtifacts,
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
import qualified Festral.SLAV.Weles.API.V1 as V1
import Festral.SLAV.Weles.API.V1 (filterID, filterName)

-- |Get list of all jobs onserver under given address.
-- This function use v2 API.
curlJobs :: NetAddress -> IO [Job]
curlJobs = V1.curlJobs

-- |Get url for download artifact by given id using V2 Weles API.
testFileUrl addr id =
    let (ip, port) = (netIP addr, netPort addr) in
        ip ++ ":" ++ show port ++ "/api/v1/artifacts/" ++ show id

-- |Get names of files of job specified by id. Always returns Just.
getFileList :: NetAddress -> Int -> IO (Maybe [String])
getFileList addr id = Just . (a_alias <$>) <$> getArtifacts addr (filterID id)

-- |Get list of artifacts of Weles job specified by filter.
getArtifacts :: NetAddress -> ArtifactFilter -> IO [Artifact]
getArtifacts = V1.getArtifacts

-- |Implementation of 'getJobOutFile' for Weles API V2.
getJobOutFile :: NetAddress -> Int -> String -> IO (Maybe String)
getJobOutFile addr id fname = do
    artifacts <- getArtifacts addr $ filterID id <&&> filterName fname
    let fileUrl = testFileUrl addr $ a_id
                $ fromMaybe emptyArtifact $ listToMaybe artifacts
    (errCode, content) <- curlGetString fileUrl [CurlFollowLocation True]
    let res = if errCode == CurlOK
            then Just content
            else Nothing
    return res
