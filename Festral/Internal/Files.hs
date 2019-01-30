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

module Festral.Internal.Files (
    freshBuilds,
    freshTests,
    buildCache,
    badFile,
    safeReadFile,
    configFile,
    getAppConfig,
    defaultConfigFileName,
    reportFilePath
) where

import System.Directory
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Control.Monad
import Data.Maybe
import Control.Exception

import Festral.Config

defaultConfigFileName = ".festral.conf"

-- |Returns path to the file with latest build names.
freshBuilds = do
    x <- getHomeDirectory
    createDirectoryIfMissing False $ x ++ "/.festral"
    return $ x ++ "/.festral/fresh_builds"

-- |Returns path to the file with build cache.
buildCache = do
    home <- getHomeDirectory
    createDirectoryIfMissing False $ home ++ "/.festral"
    return $ home ++ "/.festral/build.cachce"

-- |Returns path to the file which contains latest test names.
freshTests = do
    x <- getHomeDirectory
    createDirectoryIfMissing False $ x ++ "/.festral"
    return $ x ++ "/.festral/fresh_tests"

-- |Returns path to the festral's configuration file.
configFile = do
    home <- getHomeDirectory
    return $ home ++ "/" ++ defaultConfigFileName

-- |Returns 'AppConfig' got from file located at 'configFile'.
getAppConfig confPath = do
    exists <- doesFileExist confPath
    when (not exists) $ LB.writeFile confPath "{\n}"
    confStr <- LB.readFile confPath
    let config = decode confStr :: Maybe AppConfig
    maybe (badConfig confPath) return config
    where
        badConfig confPath = do
            putStrLn $ confPath ++ " has invalid JSON format. \
            \Use default values."
            return $ fromJust $ (decode "{}" :: Maybe AppConfig)

-- |Gives path for the parsed test report of the test specified by its id
-- (returned by 'performForallNewBuilds' function or by festral test command).
--
-- @since 1.3.4
reportFilePath cfg id = testLogDir cfg ++ "/" ++ id ++ "/report.txt"

-- |Simple handler for bad file opening.
badFile :: SomeException -> IO String
badFile _ = return ""

-- |Just do 'reafFile' with handled by 'badFile' exceprions.
safeReadFile = handle badFile . readFile
