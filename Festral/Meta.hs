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

-- |This module describes metafile used for storage build and test information.
-- See "Festral.Builder.Builder" module description for more meta
-- format details.
module Festral.Meta (
    Meta(..),
    MetaParser(..),
    MetaBase(..),
    toFile,
    readMeta,
    emptyMetaBase,
    emptyMeta,
    emptyMetaTest,
    fromMetaFile,
    isBuild,
    isTest,
    isMeta,
    ($>>),
    findField
) where

import System.IO
import Data.List.Split
import Data.List
import Data.Maybe

import Festral.Files

-- |Class which describes Parser - something that can be cunstructed from the
-- file with output of the build by function 'fromFile' and return 'Meta' parsed
-- from this build output.
class MetaParser a where
    parse :: a -> IO (Maybe Meta)
    fromFile :: FilePath -> IO a
    fromHandle :: Handle -> IO a

data MetaBase
    = MetaBase
        { board      :: String
        , buildType  :: String
        , commit     :: String
        , buildTime  :: String
        , toolchain  :: String
        , builder    :: String
        , status     :: String
        , hash       :: String
        , outDir     :: FilePath
        , repoName   :: String
        , branch     :: String
        }

-- |Representation of the meta.txt file which is used by database and wep page
-- for extracting information about build results.
data Meta
    = Meta
        { buildData  :: MetaBase
        }
    | MetaTest
        { metaData   :: MetaBase
        , tester     :: String
        , testerName :: String
        , testTime   :: String
        , testName   :: String
        , testStatus :: String
        , testDevice :: String
        }

-- |Apply function from 'MetaBase' the same way for 'Meta' and for 'MetaTest'.
f $>> (Meta base) = f base
f $>> m@MetaTest{} = f $ metaData m

buildFields =
    [ ("BOARD"          , board     ,(\m x -> m{board=x}))
    , ("BUILD_TYPE"     , buildType ,(\m x -> m{buildType=x}))
    , ("COMMIT"         , commit    ,(\m x -> m{commit=x}))
    , ("BUILD_TIME"     , buildTime ,(\m x -> m{buildTime=x}))
    , ("TOOLCHAIN"      , toolchain ,(\m x -> m{toolchain=x}))
    , ("BUILDER"        , builder   ,(\m x -> m{builder=x}))
    , ("BUILD_STATUS"   , status    ,(\m x -> m{status=x}))
    , ("BUILD_HASH"     , hash      ,(\m x -> m{hash=x}))
    , ("REPO_NAME"      , repoName  ,(\m x -> m{repoName=x}))
    , ("BRANCH"         , branch    ,(\m x -> m{branch=x}))
    , ("OUT_DIR"        , outDir    ,(\m x -> m{outDir=x}))
    ]

testFields =
    [ ("TESTER"         ,tester     ,(\m x -> m{tester=x}))
    , ("TESTER_NAME"    ,testerName ,(\m x -> m{testerName=x}))
    , ("TEST_TIME"      ,testTime   ,(\m x -> m{testTime=x}))
    , ("TEST_NAME"      ,testName   ,(\m x -> m{testName=x}))
    , ("TEST_STATUS"    ,testStatus ,(\m x -> m{testStatus=x}))
    , ("TEST_DEVICE"    ,testDevice ,(\m x -> m{testDevice=x}))
    ]

instance Show MetaBase where
    show m = unparse m buildFields

instance Show Meta where
    show m@Meta{} = show $>> m
    show m@MetaTest{} = (show $>> m) ++ (unparse m testFields)

-- |Makes 'MetaBase' with all fields empty.
emptyMetaBase = MetaBase "" "" "" "" "" "" "" "" "" "" ""

-- |Makes 'Meta' with all fields empty.
emptyMeta = Meta $ emptyMetaBase

-- |Makes 'MetaTest' with all fields empty.
emptyMetaTest = MetaTest emptyMetaBase "" "" "" "" "" ""

unparse m = foldl (\ s (name, f, _) -> s ++ name ++ "=" ++ f m ++ "\n") ""

-- |Read Meta from serialized string. If meta contains TEST_NAME, TEST_TIME and
-- TEST_STATUS fields, 'Just MetaTest' is returned, if it contains BUILD_TYPE,
-- BUILD_STATUS, BUILD_HASH and REPO_NAME 'Just Meta' is returned,
-- otherwise Nothing.
readMeta :: String -> Maybe Meta
readMeta str = chooseMeta filledMeta
    where
    filledMeta = testOnly{metaData=baseOnly}
    testOnly = fillMeta (f str) testFields emptyMetaTest
    baseOnly = fillMeta (f str) buildFields emptyMetaBase
    f x = map (splitOn "=") $ filter (isInfixOf "=") $ splitOn "\n" x

chooseMeta m@MetaTest{}
    | testName          m == ""
        || testTime     m == ""
        || testStatus   m == ""
        || testDevice   m == ""
        = chooseMeta $ Meta $>> m
    | otherwise = Just m
chooseMeta m@Meta{}
    | buildType     $>> m == ""
        || status   $>> m == ""
        || hash     $>> m == ""
        || repoName $>> m == ""
        = Nothing
    | otherwise = Just m

fillMeta str fields m = foldl (\ m' (x,_,f) -> f m' $ findField x str) m fields

-- |Finds field with given name in the given list of [name,value] pairs
-- presented as two element lists.
-- If there is no such field, returns empty 'String'.
findField :: String     -- ^ Field name
          -> [[String]] -- ^ Fields list
          -> String     -- ^ Value of the searched field
findField "" _ = ""
findField name (x:xs)
    | head x == name = last x
    | otherwise = findField name xs
findField name [] = ""

-- |Write 'Meta' to the given file
toFile :: Meta -> FilePath -> IO ()
toFile m fname = do
    writeFile fname $ show m

-- |Read 'Meta' from file. Returns 'Nothing' if meta is not valid
fromMetaFile :: FilePath -> IO Meta
fromMetaFile fname = do
    mdata <- safeReadFile fname
    return $ fromMaybe emptyMeta (readMeta mdata)

-- |Check if given meta is 'Meta' constructor
isBuild Meta{} = True
isBuild _ = False

-- |Check if given meta is 'MetaTest' constructor
isTest MetaTest{} = True
isTest _ = False

-- |Check if given meta is valid 'Meta' type
isMeta x = isBuild x || isTest x
