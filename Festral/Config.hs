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
-- Configuration file is just JSON file with fields as in "AppConfig".
module Festral.Config (
    TestConfig (..),
    AppConfig (..)
) where

import Data.Aeson
import GHC.Generics
import Control.Applicative

-- |This data structure describes test configuration for one repository
--specified by name.
data TestConfig = TestConfig
    { repo      :: String
    , yaml      :: FilePath
    , parser    :: String
    , name      :: String
    , timeout   :: Int
    } deriving (Show, Generic)

instance FromJSON TestConfig where
    parseJSON = withObject "TestConfig" $ \o -> do
        repo    <- o .: "repo"
        yaml    <- o .: "yaml"
        parser  <- o .:? "parser"   .!= "Default"
        name    <- o .:? "name"     .!= "unknown"
        timeout <- o .:? "timeout"  .!= 3600
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
