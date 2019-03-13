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

-- |Weles API implementation before v1 was released and before We
--
-- @since 1.5.0
module Festral.SLAV.Weles.API.Old (
    curlJobs,
    getFileList,
    getJobOutFile
) where

import Network.Curl.Aeson
import Network.Curl
import Data.List.Split
import Data.List
import Control.Exception

import Festral.Config
import Festral.SLAV.Weles.Data

welesAddr x = (netIP x, netPort x, netFilePort x)

-- |Get list of all jobs on server under given address.
curlJobs :: NetAddress -> IO [Job]
curlJobs addr = do
    let (ip, port, _) = welesAddr addr
    handle badCurl $ curlAesonGet (ip ++ ":" ++ show port ++ "/api/v1/jobs/")
    where
        badCurl :: CurlAesonException -> IO [Job]
        badCurl ex = putStrLn (show ex) >> return []

-- |Get url for artifact storage in V0 Weles API for job specified by id.
testFileUrl addr id =
    let (ip, _, port) = welesAddr addr in
        ip ++ ":" ++ show port ++ "/" ++ show id ++ "/TESTFILE/"

-- |Implementation of the old Weles API
getFileList :: NetAddress -> Int -> IO (Maybe [String])
getFileList addr id = do
    let fileUrl = testFileUrl addr id
    (errCode, htmlOut) <- curlGetString fileUrl [CurlFollowLocation True]
    let res = if errCode == CurlOK
            then Just (extractHrefs htmlOut)
            else Nothing
    return res
    where
        extractHrefs = (map (\x -> x!!1)) . (map (splitOn "\"")) .
            ((filter (isInfixOf "a href=")) . (splitOneOf "<>"))

-- |Implementation of 'getJobOutFile' for old Weles API.
getJobOutFile :: NetAddress -> Int -> String -> IO (Maybe String)
getJobOutFile addr id fname = do
    let fileUrl = testFileUrl addr id
    (errCode, content) <- curlGetString
                            (fileUrl ++ fname)
                            [CurlFollowLocation True]
    let res = if errCode == CurlOK
            then Just content
            else Nothing
    return res
