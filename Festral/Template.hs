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

-- |Module for generating yaml configs for Weles from templated yamls
module Festral.Template (
    generateFromTemplate,
    yamlTemplater,
    preprocess,
    TemplaterOpts(..),
    TemplateType(..)
) where

import           Data.List.Split
import           Data.List
import qualified Data.String.Utils as U
import           System.Directory
import           Data.Char
import           Control.Exception

import           Festral.Config
import           Festral.Files
import           Festral.Tests.Data

-- |Type represents types of templated fields of the yaml
data TemplateType
    = URI String                -- ^Url of the file extracted from its parameter string
    | Latest_URI String         -- ^URI for the latest built package with given name
    | RPMInstallCurrent String  -- ^Install package with given name by rpm from current build
    | RPMInstallLatest String   -- ^Install package with given name by rpm from all builds
    | FileContent FilePath      -- ^Put in this place content of the specified file
    | BuildTable String         -- ^Insert into this place HTML build report table with given id
    | TestTable String          -- ^Insert into this place HTML test report table with given id
    | Exec String               -- ^Execute command
    | ExecLog String String     -- ^Execute command and write to the log file
    | Logic PreprocessUnit      -- ^Send logic command to the preprocessor

-- |Type represents preprocessor commands tree
data PreprocessUnit
    =
    -- | IF_EQ_INSERT(config_field) (value) (string)
    PreprocessIf
        { ifField       :: String
        , ifValue       :: String
        , ifInsert      :: String
        }
    -- | IF_EQ_INSERT_ELSE(config_field) (value) (if true value) (else value)
    | PreprocessIfElse
        { ifField       :: String
        , ifValue       :: String
        , ifInsert      :: String
        , elseInsert    :: String
        }
    -- | INCLUDE(filepath)
    | PreprocessInclude String
    -- | INSERT(TestUnit_field)
    | PreprocessInsert  [String]
    | NotPreprocess String
    deriving Show

data TemplaterOpts = TemplaterOpts
    { _outDir    :: String
    , _testData  :: TestUnit
    }

extractArgs x = filter (not . isBlank) $
                split (dropDelims . dropBlanks $ oneOf  "()") x

isBlank :: String -> Bool
isBlank = all isSpace

processRow :: String -> PreprocessUnit
processRow = processRow' . extractArgs

processRow' :: [String] -> PreprocessUnit
processRow' ("IF_EQ_INSERT_ELSE":f:v:i:e:_) = PreprocessIfElse f v i e
processRow' ("IF_EQ_INSERT":f:v:i:_) = PreprocessIf f v i
processRow' ("INCLUDE":f:_) = PreprocessInclude f
processRow' ("INSERT":xs) = PreprocessInsert xs
processRow' x = NotPreprocess $ concat x

preprocess' :: TestUnit -> PreprocessUnit -> String
preprocess' t (PreprocessIf f v i) = preprocess' t (PreprocessIfElse f v i "")
preprocess' t (PreprocessIfElse f v i e) = if (t <-| f) == v then i else e
preprocess' t (PreprocessInclude x) = "##TEMPLATE_FILE " ++ x ++ "##"
preprocess' t (PreprocessInsert x) = concat $ map ((<-|) t) x
preprocess' _ (NotPreprocess x) = x

-- |Preprocess given string wiht data from given test unit. Preprocessor
-- available commands are:
--
-- @
--   IF_EQ_INSERT(config_field) (value) (string)
-- @
-- This command check if __config_field__ equals for __value__, and if yes,
-- insert into this place __string__. The first argument can be name of the
-- 'TestUnit' field.
--
-- @
--   IF_EQ_INSERT_ELSE(config_field) (value) (if true value) (else value)
-- @
-- The same as IF_EQ_INSERT, but if first two arguments are not equal, insert
-- __else value__ into this place. The first argument can be name of the
-- 'TestUnit' field.
--
-- @
--   INCLUDE(filepath)
-- @
-- Insert into this place preprocessed and untemplated contents of the given
-- filepath.
--
-- @
--   INSERT(TestUnit field)
-- @
-- Insert into this place content of the field of the 'TestUnit' with given name
-- or if there is no such field, just insert argument as string.
--
-- Fields of 'TestUnit' understandable by preprocessor are: 'repo', 'parser',
-- 'name', 'target', 'yaml'.
preprocess :: TestUnit -> String -> String
preprocess test str = unlines $ preprocess' test <$> processRow <$> lines str

x <-| "repo"    = repo $ tConfig x
x <-| "parser"  = parser $ tConfig x
x <-| "name"    = name $ tConfig x
x <-| "target"  = target x
x <-| "yaml"    = yaml $ tConfig x
x <-| s         = s

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
    config <- getAppConfig
    rpms <- catch (getDirectoryContents $ _outDir opts) dirDoesntExists
    let rpmname = take 1 $ sortBy (\a b -> length a `compare` length b) $
            filter(isInfixOf url) $ rpms
    return $ "uri: 'http://"
        ++ webPageIP config ++ ":"
        ++ show (webPagePort config)
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
    config <- getAppConfig
    cachePath <- buildCache
    cache <- safeReadFile cachePath
    let (cachedName,cachedHash) = resolvePkg $ splitOn "#" $ resolvedName $
            sortBy (\a b -> length a `compare` length b)
            $ filter (isInfixOf url) $ splitOn "\n" cache
    return $ "uri: 'http://"
        ++ webPageIP config ++ ":"
        ++ show (webPagePort config)
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
    parsedFile <- generateFromTemplate (preprocess (_testData opts) content)
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
