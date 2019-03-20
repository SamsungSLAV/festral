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

-- |Module for generating yaml test scenario file for Weles from templated
-- and preprocessed yamls.
module Festral.Template (
    generateFromTemplate,
    yamlTemplater,
    TemplaterOpts(..),
    TemplateType(..)
) where

import Data.List.Split
import Data.List
import qualified Data.String.Utils as U
import System.Directory
import Control.Exception

import Festral.Config
import Festral.Internal.Files
import Festral.Internal.Preprocessor
import Festral.Tests.Data

-- |Type represents types of templated fields of the yaml
data TemplateType
    -- |Url of the file extracted from its parameter string
    = URI String
    -- |URI for the latest built package with given name
    | Latest_URI String
    -- |Install package with given name by rpm from current build
    | RPMInstallCurrent String
    -- |Install package with given name by rpm from all builds
    | RPMInstallLatest String
    -- |Put in this place content of the specified file
    | FileContent FilePath
    -- |Insert into this place HTML build report table with given id
    | BuildTable String
    -- |Insert into this place HTML test report table with given id
    | TestTable String
    -- |Execute command
    | Exec String
    -- |Execute command and write to the log file
    | ExecLog String String

data TemplaterOpts = TemplaterOpts
    { _outDir    :: String
    , _testData  :: TestUnit
    , _fsAddr    :: NetAddress
    }

-- |Generate yaml file from template using function given as second parameter
-- for resolve templated fields
generateFromTemplate :: String -> (TemplateType -> IO String) -> IO String
generateFromTemplate yaml extractor = do
    fmap concat $ sequence $ map (\str -> if isInfixOf "TEMPLATE" str
                                            then extract extractor (words str)
                                            else return str) $ splitOn "##" yaml

extract :: (TemplateType -> IO String) -> [String] -> IO String
extract extractor ("TEMPLATE_URL":url:_) = extractor (URI url)
extract extractor ("TEMPLATE_LATEST":url:_) = extractor (Latest_URI url)
extract extractor ("TEMPLATE_RPM_INSTALL_CURRENT":packagename:_)
    = extractor (RPMInstallCurrent packagename)
extract extractor ("TEMPLATE_RPM_INSTALL_LATEST":packagename:_)
    = extractor (RPMInstallLatest packagename)
extract extractor ("TEMPLATE_FILE":filename:_)
    = extractor (FileContent filename)
extract extractor ("TEMPLATE_BUILD_TABLE":id:_) = extractor (BuildTable id)
extract extractor ("TEMPLATE_TEST_TABLE":id:_) = extractor (TestTable id)
extract extractor ("TEMPLATE_RUN_TEST":cmd)
    = extractor (ExecLog (unwords cmd) "/tmp/test.log")
extract extractor ("TEMPLATE_RUN":cmd)
    = extractor (Exec $ unwords cmd)
extract _ _ = return ""

-- |Default template resolver which implements specification of Festral
-- templates. It accepts additional output directory as first parameter.
yamlTemplater :: TemplaterOpts  -- ^ Options for parser
              -> TemplateType   -- ^ Resolved 'TemplateType' from part of text
              -> IO String      -- ^ Templated part of text
yamlTemplater opts (URI url) = do
    rpms <- catch (getDirectoryContents $ _outDir opts) dirDoesntExists
    let rpmname = take 1 $ sortBy (\a b -> length a `compare` length b) $
            filter(isInfixOf url) $ rpms
    return $ "uri: 'http://"
        ++ netIP (_fsAddr opts) ++ ":"
        ++ show (netPort $ _fsAddr opts)
        ++ "/download?file="
        ++ resolvedName rpmname
        ++ "&build="
        ++ hash
        ++ "/"
        ++ dir ++ "'"
    where
        (dir:hash:_) = reverse $ splitOn "/" (_outDir opts)

yamlTemplater opts (Latest_URI url) = do
    let outDir = _outDir opts
    cachePath <- buildCache
    cache <- safeReadFile cachePath
    let (cachedName,cachedHash) = resolvePkg $ splitOn "#" $ resolvedName $
            sortBy (\a b -> length a `compare` length b)
            $ filter (isInfixOf url) $ splitOn "\n" cache
    return $ "uri: 'http://"
        ++ netIP (_fsAddr opts) ++ ":"
        ++ show (netPort $ _fsAddr opts)
        ++ "/download?file="
        ++ cachedName
        ++ "&build="
        ++ cachedHash
        ++ "/build_res'"
    where
        resolvePkg (x:y:_) = (x,y)
        resolvePkg _ = ("","")

yamlTemplater opts (RPMInstallCurrent pkg) = do
    uri <- yamlTemplater opts (URI pkg)
    return $ yamlTemplaterRpm uri pkg
yamlTemplater opts (RPMInstallLatest pkg) = do
    uri <- yamlTemplater opts (Latest_URI pkg)
    return $ yamlTemplaterRpm uri pkg
yamlTemplater opts (FileContent fname) = do
    content <- safeReadFile fname
    preprocessed <- preprocess (_testData opts) content
    parsedFile <- generateFromTemplate preprocessed
        $ yamlTemplater opts
    return parsedFile
yamlTemplater _ (ExecLog cmd logfile) = return $
    "- run:\n\
    \                  name: \"'" ++ cmd ++ " 2>&1 >> " ++ logfile ++ "'\""
yamlTemplater _ (Exec cmd) = return $
    "- run:\n\
    \                  name: \"'" ++ cmd ++ "'\""

yamlTemplaterRpm  uri package =
        "- push:\n"
    ++ "                  " ++ uri ++ "\n"
    ++ "                  dest: '/tmp/" ++ rpmName ++ "'\n"
    ++ "                  alias: '" ++ rpmName ++ "'\n"
    ++ "              - run:\n"
    ++ "                  name: \"'rpm -i /tmp/"
    ++ rpmName ++ " --force 2>&1 >> /tmp/install.log'\""
    where
        rpmName = package ++ ".rpm"

dirDoesntExists :: SomeException -> IO [FilePath]
dirDoesntExists ex = putStrLn (show ex) >> return []

resolvedName x = if x == [] then "" else head x
