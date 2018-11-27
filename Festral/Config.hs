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

-- |This module describes Festral's configuration file structure and
-- serialization methods.
-- Configuration file is just JSON file with fields as in 'AppConfig'.
module Festral.Config (
    TestConfig (..),
    AppConfig (..),
    TestUnit (..)
) where

import Data.Aeson
import GHC.Generics
import Control.Applicative

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

-- |Describes basic festral application configuration. There are default values
-- of "AppConfig" read from JSON listed below:
data AppConfig = AppConfig
    { buildLogDir   :: FilePath -- ^Default: \/tmp\/builds
    , testLogDir    :: FilePath -- ^Default: \/tmp\/tests
    , welesIP       :: String   -- ^Default: 127.0.0.1
    , welesPort     :: Int      -- ^Default: 8888
    , welesFilePort :: Int      -- ^Default: 8888
    , webPageIP     :: String   -- ^Default: 127.0.0.1
    , webPagePort   :: Int      -- ^Default: 8888
    , serverRoot    :: FilePath -- ^Default: \/tmp\/festral_server
    , borutaIP      :: String   -- ^Default: 127.0.0.1
    , borutaPort    :: Int      -- ^Default: 6666
    } deriving (Show, Generic)

instance FromJSON AppConfig where
    parseJSON = withObject "AppConfig" $ \o -> do
        buildLogDir     <- o .:? "buildLogDir"  .!= "/tmp/builds"
        testLogDir      <- o .:? "testLogDir"   .!= "/tmp/tests"
        welesIP         <- o .:? "welesIP"      .!= "127.0.0.1"
        welesPort       <- o .:? "welesPort"    .!= 8888
        welesFilePort   <- o .:? "welesFilePort".!= 8888
        webPageIP       <- o .:? "festralIP"    .!= "127.0.0.1"
        webPagePort     <- o .:? "festralPort"  .!= 8888
        serverRoot      <- o .:? "serverRoot"   .!= "/tmp/festral_server"
        borutaIP        <- o .:? "borutaIP"     .!= "127.0.0.1"
        borutaPort      <- o .:? "borutaPort"   .!= 6666
        return AppConfig{..}
instance ToJSON AppConfig
