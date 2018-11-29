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

module Festral.Files (
    freshBuilds,
    freshTests,
    buildCache,
    badFile,
    safeReadFile,
    getAppConfig
) where

import System.Directory
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Festral.Config
import Control.Monad
import Data.Maybe
import Control.Exception

freshBuilds = do
    x <- getHomeDirectory
    createDirectoryIfMissing False $ x ++ "/.festral"
    return $ x ++ "/.festral/fresh_builds"

buildCache = do
    home <- getHomeDirectory
    createDirectoryIfMissing False $ home ++ "/.festral"
    return $ home ++ "/.festral/build.cachce"

freshTests = do
    x <- getHomeDirectory
    createDirectoryIfMissing False $ x ++ "/.festral"
    return $ x ++ "/.festral/fresh_tests"

configFile = do
    home <- getHomeDirectory
    return $ home ++ "/.festral.conf"

getAppConfig = do
    confPath <- configFile
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

-- |Simple handler for bad file opening.
badFile :: SomeException -> IO String
badFile _ = return ""

-- |Just do 'reafFile' with handled by 'badFile' exceprions.
safeReadFile = handle badFile . readFile
