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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Module with data types needed for "Festral.Tests.Test" module.
module Festral.Tests.Data (
    TestResult(..),
    TestStatus(..),
    FileContents,
    JobStartResult(..),
    JobExecutionResult(..),
    TestConfig (..),
    TestUnit (..),
    showJobResultId
) where

import Data.Aeson
import GHC.Generics

-- |This data structure describes test configuration for one repository
--specified by name.
data TestConfig = TestConfig
    {
    -- |Repository name. Required.
      repo      :: String
    -- |Path to the YAML file with test description. Required.
    , yaml      :: FilePath
    -- |Name of built-in parser or path to the own test parser executable.
    -- Default valie: \"Default\"
    , parser    :: String
    -- |Test name which will be displayed in the reports. Default value:
    -- \"unknown\"
    , name      :: String
    -- |Time to live of test job from the moment it was created in seconds.
    -- After this limit expired job will be cancelled even if it was just
    -- waiting in queue and has not started execution. This limit is needed for
    -- force cancellation of jobs if execution time of all jobs is larger then
    -- running next tests iteration starts. See 'runTTL' to limit execution
    -- time.
    , timeout   :: Int
    -- |Time to live of test job from its execution on the target started
    -- in seconds. This option is needed for limiting of execution time
    -- of test. Test job will be cancelled after one of the 'timeout' or
    -- 'runTTL' will expired.
    , runTTL    :: Int
    -- |List of target device types which this test need to be executed on.
    , targets   :: [String]
    } deriving (Show, Generic)

-- |Single test description created for each target device for specified test.
data TestUnit = TestUnit
    { tConfig   :: TestConfig
    , target    :: String
    } deriving Show

-- |List of pairs filename - content
type FileContents = [(String, String)]

data TestResult
    = TestResult
        { testStatus :: TestStatus
        , testConfig :: TestUnit
        }
    deriving Show

data TestStatus
    -- |Appears when segmantation fault detected during testing process.
    -- Contains test execution logs.
    = SegFault FileContents
    -- |Appears when job finished with error
    | BadJob JobExecutionResult
    -- |If test job finished withowt errors, it returns 'TestSuccess'.
    -- It doesn't mean that tests was passed. Contains test logs.
    | TestSuccess FileContents

instance Show TestStatus where
    show (SegFault _) = "SEGFAULT"
    show (BadJob x) = show x
    show (TestSuccess _) = "COMPLETE"

-- |Status of the starting of test job.
data JobStartResult
    -- |Job was not started because repository under test failed to build
    = BuildFailed
    -- |YAML file passed to the Weles does not exist
    | BadYaml
    -- |Job was not able to start by some reason
    | StartJobFailed
    -- |If job was started successfully, its id is returned.
    -- Contains usual job's ID
    | JobId Int

-- |Status of test job execution.
data JobExecutionResult
    -- |After successfull completion of testing logs are returned by this
    -- constructor. Contains logs and job start result.
    = JobLogs FileContents JobStartResult
    -- |Job execution failed because Dryad failed with error. It usually means
    -- some hardware error (connection between MuxPi and DUT were lost | some
    -- commands executed on DUT failed | DUT was flashed with bad OS image etc.)
    -- Contains logs and job start result.
    | DryadError FileContents JobStartResult
    -- |Weles failed to download some files specified in YAML file (maybe link
    -- is invalid or server has no free space). Contains job start result.
    | DownloadError JobStartResult
    -- |Other unexpected error. Contains error message and job start result.
    | UnknownError String JobStartResult
    -- |Connection for the Weles was lost.
    | ConnectionLost JobStartResult
    -- |Job was not started.
    | JobNotStarted JobStartResult
    -- |Job was timed out and cancelled by Weles
    | Cancelled FileContents JobStartResult

instance Show JobStartResult where
    show BuildFailed        = "BUILD FAILED"
    show BadYaml            = "YAML NOT FOUND"
    show StartJobFailed     = "NO JOB STARTED"
    show (JobId x)          = show x

instance Show JobExecutionResult where
    show (JobLogs x _)      = show x
    show (DryadError{})     = "DEVICE FAILED"
    show (DownloadError{})  = "DOWNLOAD FILES ERROR"
    show (UnknownError{})   = "WELES ERROR"
    show (ConnectionLost{}) = "CONNECTION LOST"
    show (JobNotStarted x)  = show x
    show (Cancelled{})      = "CANCELLED"

instance FromJSON TestConfig where
    parseJSON = withObject "TestConfig" $ \o -> do
        repo    <- o .: "repo"
        yaml    <- o .: "yaml"
        parser  <- o .:? "parser"   .!= "Default"
        name    <- o .:? "name"     .!= "unknown"
        timeout <- o .:? "timeout"  .!= 3600
        runTTL  <- o .:? "runTTL"   .!= 1200
        targets <- o .:? "targets"  .!= []
        return TestConfig{..}

instance ToJSON TestConfig

showJobResultId (JobLogs _ i)       = show i
showJobResultId (DryadError _ i)    = show i
showJobResultId (DownloadError i)   = show i
showJobResultId (UnknownError _ i)  = show i
showJobResultId (ConnectionLost i)  = show i
showJobResultId (JobNotStarted i)   = show i
showJobResultId (Cancelled _ i)     = show i
